import Foundation
import FrontEnd

extension IR.Program {

  /// Elaborates all functions in `self` that call projections to use regular function calls.
  public mutating func elaborateProjectionCallers() {
    for m in modules.values {
      elaborateProjectionCallers(in: m.id)
    }
  }

  /// Elaborates all functions in `m` that call projections to use regular function calls.
  private mutating func elaborateProjectionCallers(in m: Module.ID) {
    for f in modules[m]!.functions.keys {
      guard modules[m]![f].instructions.contains(where: { $0 is Project }) else { continue }
      elaborateProjectionCaller(f, in: m)
    }
  }

  /// Elaborates the projection caller function `f` in module `m`.
  private mutating func elaborateProjectionCaller(_ f: Function.ID, in m: Module.ID) {
    // TODO: remove this filter.
    if !Module.canLowerProjections(modules[m]![f].site.file) {
      return
    }

    // Before doing anything, remove all the instructions not needed for code generation.
    modules[m]![f].removeSemanticallyTransparentDefinitions()

    // Determine the scopes that need lowering.
    let source = modules[m]![f]
    var scopes = source.projectionCallingScopes(id: f)

    // Create the frame; one frame for the entire function.
    var e = FrameMaterializationInfo()
    for d in scopes {
      // Look only in plateau regions; that is skip the first and last regions, which are outside of the plateau.
      for r in 1 ..< d.regionsCount - 1 {
        let s = d.splitInstruction(startingRegion: r)
        let a = d.instructions(region: r)
        e.collectCrossRegionInstructions(in: source, from: a, ignoring: [s])
      }
    }
    let frame = modules[m]!.materialize(&e, in: f)

    // The instructions have changed, so we need to recompute the scopes.
    scopes = modules[m]![f].projectionCallingScopes(id: f)

    // Elaborate each caller scope.
    for d in scopes {
      elaborateCallerScope(d, frame: frame, in: m)
    }
  }

  /// Elaborates the projection calling scope described by `d` in module `m`.
  private mutating func elaborateCallerScope(
    _ d: ScopeDetails, frame: Operand?, in m: Module.ID
  ) {
    // TODO: handle cases with more than one plateau.
    precondition(
      d.regionsCount == 3, "multiple projection uses in a caller scope not supported yet")
    let plateau = generatePlateauFunction(for: d, region: 1, plateauIndex: 0, frame: frame, in: m)

    // Replace the old region with the elaborated code that calls the elaborated projection.
    modules[m]!.replacePlateau(d, region: 1, withPlateau: plateau, frame: frame)
  }

  /// Generates the plateau function for region `index` of caller scope `d` in module `m`,
  /// returning its identity.
  private mutating func generatePlateauFunction(
    for d: ScopeDetails,
    region index: Int,
    plateauIndex: Int,
    frame: Operand?,
    in m: Module.ID
  ) -> Function.ID {
    let source = modules[m]![d.id]

    // Generate the declaration.
    let oldProject = d.splitInstruction(startingRegion: index)
    let plateau = modules[m]!.demandPlateau(
      for: d.id, region: plateauIndex, projectedType: source[oldProject].result!.ast)

    // Add the entry block in the plateau function.
    let entry = modules[m]![plateau].appendBlock(in: source[d.blocks[0]].scope)
    let rewrittenBlocks: [Block.ID: Block.ID] = [
      d.blocks[0]: entry
    ]

    var rewrittenRegisters: [Operand: Operand] = [
      .register(oldProject): .parameter(entry, 0)
    ]

    // If we have a frame, take it from the function parameter.
    if let f = frame {
      let t = modules[m]![d.id].type(of: f).ast
      let frameParameter = Operand.parameter(entry, 1)
      let ourFrame = modules[m]!.modifyIR(of: plateau, at: .end(of: entry)) { (e) in
        let x0 = e._load(frameParameter)
        return e._pointer_to_place(x0, as: RemoteType(.inout, t))
      }
      rewrittenRegisters[f] = ourFrame
    }

    var transformer = DictionaryInstructionTransformer(
      rewrittenBlocks: rewrittenBlocks, rewrittenRegisters: rewrittenRegisters)

    // Copy all the relevant instructions to the new plateau function.
    rewrite(
      d.instructions(region: index), in: d.id, from: m, transformedBy: &transformer, to: plateau)

    // Generate the call to the continuation (which calls the projection slide).
    let lastBlock = transformer.rewrittenBlocks[d.blocks.last!]!
    modules[m]!.generateContinuationCall(toBlock: lastBlock, of: plateau)

    return plateau
  }

}

extension Module {

  /// Replaces in scope `d` the region `index` by a call to the appropriate projection ramp,
  /// passing a continuation to the plateau `p` to be executed.
  fileprivate mutating func replacePlateau(
    _ d: ScopeDetails, region index: Int, withPlateau p: Function.ID, frame: Operand?
  ) {
    let oldProject = d.splitInstruction(startingRegion: index)
    let oldEndProject = d.splitInstruction(endingRegion: index)

    let oldProjectIR = self[d.id][oldProject]
    let t = oldProjectIR.result!.ast
    let ramp = Function.ID(projectionRamp: (oldProjectIR as! Project).callee)
    let rampReference = FunctionReference(to: ramp, in: self)
    let plateauReference = FunctionReference(to: p, in: self)

    modifyIR(of: d.id, at: .before(oldEndProject)) { (e) in
      let x0 = frame ?? e._call_builtin(.zeroinitializer(BuiltinType.ptr), [])
      let x1 = e._place_to_pointer(x0)
      let c = e._plateauContinuation(calling: plateauReference, frame: x1, projectedType: t)
      e._callProjectionRamp(
        rampReference, with: oldProjectIR.operands, continuation: c)
    }

    // Remove old instructions...
    self[d.id].remove(oldEndProject)
    for i in d.instructions(region: index).reversed() {
      self[d.id].remove(i)
    }
    self[d.id].remove(oldProject)
  }

  /// Generates the code in block `b` of `f` to call the continuation received as the third
  /// parameter to `f`, followed by a return instruction.
  ///
  /// The plateau function `f` has the following signature:
  ///
  ///     fun Caller.plateauN(inout <yield-type>, let Builtin.ptr, let ProjectionContinuation) -> {}
  fileprivate mutating func generateContinuationCall(
    toBlock b: Block.ID, of f: Function.ID
  ) {
    let entry = self[f].entry!
    modifyIR(of: f, at: .end(of: b)) { (e) in
      e._resumeContinuation(Operand.parameter(entry, 2))
      e._return()
    }
  }

}

extension Emitter {

  /// Allocates and initializes a continuation to call `plateau` with frame pointer `f` for a
  /// projected value of type `t`.
  fileprivate mutating func _plateauContinuation(
    calling plateau: FunctionReference, frame f: Operand,
    projectedType t: AnyType
  ) -> Operand {
    let x0 = _place_to_pointer(Operand.constant(plateau))
    let x1 = _alloc_stack(Module.plateauContinuationType(projectedType: t))
    let x2 = _subfield_view(x1, at: [0, 0])
    _store(x0, x2)
    let x3 = _subfield_view(x1, at: [0, 1])
    _store(f, x3)
    return x1
  }

  /// Generates IR for jumping to projection continuation `c`.
  fileprivate mutating func _resumeContinuation(_ c: Operand) {
    let x0 = _subfield_view(c, at: [0])  // c.resumeFunction
    let x1 = _subfield_view(c, at: [1])  // c.frame
    let x2 = _alloc_stack(.void)
    _emitApply(x0, to: [x1], writingResultTo: x2)
  }

  /// Emits a call to the projection ramp `r` with `arguments` and `continuation`.
  fileprivate mutating func _callProjectionRamp(
    _ r: FunctionReference, with arguments: [Operand], continuation: Operand
  ) {
    let x1 = _alloc_stack(.void)
    _emitApply(Operand.constant(r), to: arguments + [continuation], writingResultTo: x1)
  }

}

extension Function {

  /// Identifies the scopes that need to be elaborated and returns their details.
  fileprivate func projectionCallingScopes(id: Function.ID) -> [ScopeDetails] {
    // TODO: handle multiple scopes in a caller function.
    // The blocks need to be in dominance order.
    let blocks = Array(cfg().iterateFrom([entry!]))
    return [ScopeDetails(id, source: self, blocks: blocks)]
  }

}

/// The details needed to elaborate a scope using projections.
private struct ScopeDetails {

  /// The identifier for the original caller function to be elaborated.
  let id: Function.ID

  /// All the blocks that need to be elaborated, which are in dominance order.
  let blocks: [Block.ID]

  /// All the instructions in `blocks`.
  let instructions: [InstructionID]

  /// The split positions in `instructions`, corresponding to `project` and `end_project`
  /// instructions, separating our blocks into regions.
  let regionSplitPositions: [Int]

  /// The number of regions in the caller scope.
  var regionsCount: Int {
    regionSplitPositions.count + 1
  }

  /// Returns the split instruction at the end of region `r`.
  ///
  /// - Requires: `r < regionsCount - 1`.
  internal func splitInstruction(endingRegion r: Int) -> InstructionID {
    precondition(r < regionsCount - 1)
    return instructions[regionSplitPositions[r]]
  }

  /// Returns the split instruction at the start of region `r`.
  ///
  /// - Requires: `0 < r < regionsCount`.
  internal func splitInstruction(startingRegion r: Int) -> InstructionID {
    precondition(0 < r && r < regionsCount)
    return instructions[regionSplitPositions[r - 1]]
  }

  /// Returns all the instructions in region `r`, excluding the split instructions.
  internal func instructions(region r: Int) -> Array<InstructionID>.SubSequence {
    precondition(r < regionsCount)
    let start = r == 0 ? instructions.startIndex : regionSplitPositions[r - 1] + 1
    let end = r < regionSplitPositions.count ? regionSplitPositions[r] : instructions.endIndex
    return instructions[start ..< end]
  }

  /// An instance describing projection caller `f`, with IR `source`, having `blocks` to be elaborated.
  ///
  /// - Precondition: all `blocks` are blocks of `source`.
  /// - Precondition: `blocks` is sorted by dominance order.
  init(_ f: Function.ID, source: Function, blocks: [Block.ID]) {
    self.id = f
    self.blocks = blocks
    let xs = blocks.flatMap({ source.instructions(in: $0) })
    self.instructions = xs
    self.regionSplitPositions = xs.indices.compactMap { (p: Int) in
      let x = xs[p]
      return (source[x] is Project || source[x] is EndProject) ? p : nil
    }
  }

}
