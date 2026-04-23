import Foundation
import FrontEnd

extension IR.Program {

  /// Elaborates all projections in `self` into ramp + slide functions.
  public mutating func elaborateProjections() {
    for m in modules.values {
      elaborateProjections(in: m.id)
    }
  }
  /// Elaborates the projections in `m` into a pair of ramp+slide functions.
  private mutating func elaborateProjections(in m: Module.ID) {
    for f in modules[m]!.functions.keys where modules[m]![f].isSubscript {
      guard !modules[m]![f].blockIDs.isEmpty else { continue }
      guard let p = modules[m]!.projectionSkeletons[f] else {
        fatalError(
          "missing projection skeleton for projection function \(modules[m]!.debugDescription(f))")
      }
      elaborateProjection(f, skeleton: p, in: m)
    }
  }

  /// Elaborates the projection function `f` with skeleton `skeleton` in module `m`.
  private mutating func elaborateProjection(
    _ f: Function.ID, skeleton s: IR.ProjectionSkeleton, in m: Module.ID
  ) {
    // TODO: remove this filter.
    if !Module.canLowerProjections(modules[m]![f].site.file) {
      return
    }

    modules[m]![f].removeGhostDefinitions()
    
    let ramp = modules[m]!.demandProjectionRamp(for: f)
    let slide = modules[m]!.demandProjectionSlide(for: f)
    let source = modules[m]![f]

    // Create the frame.
    var details = ProjectionDetails(f, source: source, skeleton: s, of: base)
    var e = FrameMaterializationInfo()
    e.collectCrossRegionInstructions(in: source, from: details.slideInstructions, ignoring: [])
    let frame = modules[m]!.materialize(&e, in: f)

    // The instructions have changed, so we need to recompute the details.
    details = ProjectionDetails(f, source: modules[m]![f], skeleton: s, of: base)

    constructRamp(ramp, for: details, slide: slide, frame: frame, in: m)
    constructSlide(slide, for: details, frame: frame, in: m)
    modules[m]!.removeFunction(f)
  }

  /// Generate the body of `ramp`, from projection with details `d`, referencing `slide`, in module `m`.
  private mutating func constructRamp(
    _ ramp: Function.ID, for d: ProjectionDetails, slide: Function.ID,
    frame: Operand?,
    in m: Module.ID
  ) {
    var transformer = DictionaryInstructionTransformer()
    let source = modules[m]![d.id]

    // Copy the ramp instructions, creating blocks for them as needed.
    rewrite(d.rampInstructions, in: d.id, from: m, transformedBy: &transformer, to: ramp)
    // The frame operand is rewritten in the process.
    let rewrittenFrame = frame.map({ transformer.transform($0) })

    // Add the projected value storage at the beginning of the entry block.
    let entry = modules[m]![ramp].entry!
    let s = modules[m]!.modifyIR(of: ramp, at: .start(of: entry)) { (e) in
      e._alloc_stack(source.output)
    }

    // Generate the last block that jumps to the continuation passed in by the caller, and exits.
    let b = modules[m]!.generateContinuationCall(
      in: ramp, referencing: slide, projecting: s, frame: rewrittenFrame)

    // Add yield replacements.
    for y in d.skeleton.yieldPoints {
      modules[m]!.addYieldReplacement(
        yield: y, for: d, in: ramp, transformedBy: &transformer, jumpingTo: b,
        projectedValueStorage: s)
    }
  }

  /// Generate the body of `slide`, from projection with details `d`, in module `m`.
  private mutating func constructSlide(
    _ slide: Function.ID, for d: ProjectionDetails, frame: Operand?,
    in m: Module.ID
  ) {
    // If all the slide blocks contain just terminators, the whole slide is empty.
    if modules[m]!.slideIsEmpty(projection: d) {
      modules[m]!.generateEmptyBody(for: slide, copying: d.id)
      return
    }

    // Create the entry block.
    let slideEntry = modules[m]!.createSlideEntryBlock(slide: slide, for: d)

    var rewrittenRegisters: [Operand: Operand] = [:]

    // If we have a frame, take it from the function parameter.
    if let f = frame {
      let t = modules[m]![d.id].type(of: f).ast
      let frameParameter = Operand.parameter(slideEntry, 0)
      let ourFrame = modules[m]!.modifyIR(of: slide, at: .end(of: slideEntry)) { (e) in
        let x0 = e._load(frameParameter)
        return e._pointer_to_place(x0, as: RemoteType(.inout, t))
      }
      rewrittenRegisters[f] = ourFrame
    }

    var transformer = DictionaryInstructionTransformer(
      rewrittenRegisters: rewrittenRegisters)

    // Copy the slide instructions, creating blocks for them as needed.
    rewrite(d.slideInstructions, in: d.id, from: m, transformedBy: &transformer, to: slide)

    // Fill the entry block.
    modules[m]!.fillSlideEntryBlock(slideEntry, in: slide, for: d, transformedBy: &transformer)
  }

}

extension Module {

  /// Inserts a replacement for the yield instruction `y` of projection `d` into `ramp`, using `t`
  /// to map between the original projection and the ramp.
  fileprivate mutating func addYieldReplacement(
    yield y: InstructionID,
    for d: ProjectionDetails,
    in ramp: Function.ID,
    transformedBy t: inout DictionaryInstructionTransformer,
    jumpingTo continuationBlock: Block.ID,
    projectedValueStorage: Operand
  ) {
    let source = self[d.id]
    let sourceYield = source[y] as! Yield
    let b = t.rewrittenBlocks[source.block(of: y)]!

    // Insert code at the yield point.
    modifyIR(of: ramp, at: .end(of: b)) { (e) in
      // If we have multiple yields, store the index of the current yield point in the state.
      if d.skeleton.yieldPoints.count > 1 {
        _ /*index*/ = d.skeleton.yieldPoints.firstIndex(of: y)!
        // TODO: store `index` in the frame.
      }

      // Store the yield value in the storage for projected value.
      let x0 = e._load(t.transform(sourceYield.projection))
      e._store(x0, projectedValueStorage)

      // Jump to the continuation block.
      e._branch(to: continuationBlock)
    }
  }

  /// Generates a new block in `ramp` that calls the continuation received as parameter, passing
  /// the value from `p` and a continuation that calls `slide`; returns the identity of the new block.
  fileprivate mutating func generateContinuationCall(
    in ramp: Function.ID,
    referencing slide: Function.ID,
    projecting p: Operand,
    frame: Operand?
  ) -> Block.ID {
    let slideReference = FunctionReference(to: slide, in: self)
    let continuationParameter = continuationParameter(ramp: ramp)
    let b = self[ramp].appendBlock(in: self[ramp][self[ramp].entry!].scope)
    modifyIR(of: ramp, at: .end(of: b)) { (e) in
      let x0 = frame ?? e._call_builtin(.zeroinitializer(BuiltinType.ptr), [])
      let x1 = e._place_to_pointer(x0)
      let c = e._slideContinuation(calling: slideReference, frame: x1)
      e._resumeContinuation(continuationParameter, with: c, projecting: p)
      e._return()
    }
    return b
  }

  /// Returns the operand representing the continuation parameter in ramp `f`.
  ///
  /// The ramp function has the following signature:
  ///
  ///     fun Projection.ramp(<parameters>, _ c: let PlateauContinuation) -> {}
  ///
  /// The entry block of the ramp function will look like:
  ///
  ///     b0(<parameters>, %b0#N1 : &<PlateauContinuation>, %b0#N : &{}):
  ///
  /// adding an extra parameter for the return type.
  ///
  /// We will return the previous-to-last parameter of the block.
  private func continuationParameter(ramp f: Function.ID) -> Operand {
    let source = self[f]
    let entry = source.entry!
    return .parameter(entry, source[entry].inputs.count - 2)
  }

  /// Returns `true` if there is no useful code to execute in the slide of projection `p`.
  fileprivate func slideIsEmpty(projection p: ProjectionDetails) -> Bool {
    let source = self[p.id]
    return p.slideInstructions.allSatisfy({ source[$0] is Terminator })
  }

  /// Generates an empty body for function `f`, copying the entry block's scope from `g`.
  fileprivate mutating func generateEmptyBody(for f: Function.ID, copying g: Function.ID) {
    precondition(f != g)
    let source = self[g]
    let entry = self[f].appendBlock(in: source[source.entry!].scope)
    modifyIR(of: f, at: .end(of: entry)) { (e) in
      e._return()
    }
  }

  /// Creates entry block in `slide`, copying the scope from the projection `d`, returning its identity.
  fileprivate mutating func createSlideEntryBlock(
    slide: Function.ID, for d: ProjectionDetails
  ) -> Block.ID {
    let source = self[d.id]
    return self[slide].appendBlock(in: source[source.entry!].scope)
  }

  /// Fills the entry block `b` in `slide` for projection `d`, using `t` to map between the original
  /// projection and the slide.
  fileprivate mutating func fillSlideEntryBlock(
    _ b: Block.ID, in slide: Function.ID, for d: ProjectionDetails,
    transformedBy t: inout DictionaryInstructionTransformer
  ) {
    // TODO: if we have multiple yields, jump to the right block based on the index stored in the frame.
    let source = self[d.id]
    let firstYield = d.skeleton.yieldPoints[0]
    let targetBlock = t.rewrittenBlocks[source.block(of: firstYield)]!
    modifyIR(of: slide, at: .end(of: b)) { (e) in
      e._branch(to: targetBlock)
    }
  }

}

extension Emitter {

  /// Allocates and initializes a continuation to call `slide` with frame pointer `f`.
  fileprivate mutating func _slideContinuation(
    calling slide: FunctionReference, frame f: Operand
  ) -> Operand {
    let x0 = _place_to_pointer(Operand.constant(slide))
    let x1 = _alloc_stack(Module.projectionContinuationType())
    let x2 = _subfield_view(x1, at: [0, 0])
    _store(x0, x2)
    let x3 = _subfield_view(x1, at: [0, 1])
    _store(f, x3)
    return x1
  }

  /// Emit code that jumps to continuation `c`, passing `slideContinuation` as argument.
  fileprivate mutating func _resumeContinuation(
    _ c: Operand, with slideContinuation: Operand,
    projecting s: Operand
  ) {
    let x0 = _subfield_view(c, at: [0])  // c.resumeFunction
    let x1 = _subfield_view(c, at: [1])  // c.frame
    let x2 = _alloc_stack(.void)
    _emitApply(x0, to: [s, x1, slideContinuation], writingResultTo: x2)
  }
}

/// The details needed to elaborate a projection function.
private struct ProjectionDetails {

  /// The projection's identifier.
  let id: Function.ID

  /// The skeleton of the projection.
  let skeleton: ProjectionSkeleton

  /// All the instructions in the ramp.
  /// May contain instructions from different blocks.
  let rampInstructions: [InstructionID]

  /// All the instructions in the slide.
  /// May contain instructions from different blocks.
  let slideInstructions: [InstructionID]

  /// An instance describing projection `p`, with IR `source` and skeleton `s`.
  init(
    _ p: Function.ID, source: Function, skeleton s: ProjectionSkeleton, of program: TypedProgram
  ) {
    var rampInstructions = s.rampBlocks.flatMap { source.instructions(in: $0) }
    var slideInstructions = s.slideBlocks.flatMap { source.instructions(in: $0) }

    for y in s.yieldPoints {
      let a = Array(source.instructions(in: source.block(of: y)))
      let s = a.firstIndex(of: y)!
      rampInstructions.append(contentsOf: a[0 ..< s])
      // Note: `yield` is not included neither in the ramp nor in the slide.
      slideInstructions.append(contentsOf: a[(s + 1)...])
    }

    self.id = p
    self.skeleton = s
    self.rampInstructions = rampInstructions
    self.slideInstructions = slideInstructions
  }

}
