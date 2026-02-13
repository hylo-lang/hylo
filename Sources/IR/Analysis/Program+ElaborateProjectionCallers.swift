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
    if modules[m]![f].site.file.baseName != "projection" { return }  // TODO: remove this filter.

    let source = modules[m]![f]
    for d in source.projectionCallingScopes(id: f) {
      elaborateCallerScope(d, in: m)
    }
  }

  /// Elaborates the projection calling scope described by `d` in module `m`.
  private mutating func elaborateCallerScope(_ d: ScopeDetails, in m: Module.ID) {
    // TODO: handle cases with more than one plateau.
    precondition(
      d.regionsCount == 3, "multiple projection uses in a caller scope not supported yet")
    let plateau = generatePlateauFunction(for: d, region: 1, plateauIndex: 0, in: m)

    // Replace the old region with the elaborated code that calls the elaborated projection.
    modules[m]!.replacePlateau(d, region: 1, withPlateau: plateau)
  }

  /// Generates the plateau function for region `index` of caller scope `d` in module `m`,
  /// returning its identity.
  private mutating func generatePlateauFunction(
    for d: ScopeDetails, region index: Int, plateauIndex: Int, in m: Module.ID
  ) -> Function.ID {
    let source = modules[m]![d.id]

    // Generate the declaration.
    let plateau = modules[m]!.demandCallerPlateauDeclaration(for: d.id, region: plateauIndex)

    // Add the entry block in the plateau function.
    let entry = modules[m]![plateau].appendBlock(in: source[d.blocks[0]].scope)
    let rewrittenBlocks: [Block.ID: Block.ID] = [
      d.blocks[0]: entry
    ]

    // Add storage for the projected value in the plateau, to replace the projected value.
    let oldProject = d.splitInstruction(endingRegion: index - 1)
    let p = modules[m]!.addStorage(in: entry, of: plateau, replacing: source[oldProject])
    let rewrittenRegisters: [Operand: Operand] = [
      .register(oldProject): .register(p)
    ]

    // From the tail we need to ignore (not rewrite) the uses of the projected value.
    // TODO: make this work for the general case
    let oldProjectAccess = (source[oldProject] as! Project).operands[0].instruction!
    let toIgnore = source.allUses(of: oldProjectAccess).map({ $0.user })

    var transformer = DictionaryInstructionTransformer(
      rewrittenBlocks: rewrittenBlocks, rewrittenRegisters: rewrittenRegisters)

    // Copy all the relevant instructions to the new plateau function.
    rewrite(d.prefixInstructions(region: index), in: d.id, from: m, transformedBy: &transformer, to: plateau)
    let tail = d.epilogueInstructions(region: index).filter({ !toIgnore.contains($0) })
    rewrite(tail, in: d.id, from: m, transformedBy: &transformer, to: plateau)

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
    _ d: ScopeDetails, region index: Int, withPlateau p: Function.ID
  ) {
    let oldProject = d.splitInstruction(endingRegion: index - 1)
    let oldEndProject = d.splitInstruction(endingRegion: index)

    let oldProjectIR = self[d.id][oldProject]
    let t = oldProjectIR.result!.ast
    let ramp = Function.ID(projectionRamp: (oldProjectIR as! Project).callee)
    let rampReference = FunctionReference(to: ramp, in: self)
    let plateauReference = FunctionReference(to: p, in: self)

    modifyIR(of: d.id, at: .before(oldEndProject)) { (e) in
      let nullCall = e._call_builtin(.zeroinitializer(BuiltinType.ptr), [])
      let c = e._plateau_continuation(calling: plateauReference, frame: nullCall)
      e._call_projection_ramp(
        rampReference, with: oldProjectIR.operands, frameValueType: t, continuation: c)
    }

    // Remove old instructions...
    var toRemove = [oldProject]
    var deallocs: [InstructionID] = []
    for i in d.instructions(region: index) {
      toRemove.append(i)
      for u in self[d.id].allUses(of: i) where self[d.id][u.user] is DeallocStack {
        deallocs.append(u.user)
      }
    }
    for i in [toRemove, deallocs].joined().reversed() {
      self[d.id].remove(i)
    }
  }

  /// Creates storage in `f` for the value represented by `i` and returns its identity.
  fileprivate mutating func addStorage(
    in b: Block.ID, of f: Function.ID, replacing i: Instruction
  ) -> InstructionID {
    let t = i.result!.ast
    let r = modifyIR(of: f, at: .end(of: b)) { (e) in
      e._alloc_stack(t)
    }
    return r.instruction!
  }

  /// Generates the code in block `b` of `f` to call the continuation received as the second
  /// parameter to `f`, followed by a return instruction.
  fileprivate mutating func generateContinuationCall(
    toBlock b: Block.ID, of f: Function.ID
  ) {
    let entryBlock = self[f].entry!
    modifyIR(of: f, at: .end(of: b)) { (e) in
      e._resume_continuation(Operand.parameter(entryBlock, 1))
      e._return()
    }
  }

}

extension Emitter {

  /// Allocates and initializes a continuation to call `plateau` with frame pointer `f`.
  fileprivate mutating func _plateau_continuation(
    calling plateau: FunctionReference, frame f: Operand
  ) -> Operand {
    let x0 = _place_to_pointer(Operand.constant(plateau))
    let x1 = _alloc_stack(Module.callerContinuationType())
    let x2 = _access(.set, from: _subfield_view(x1, at: [0, 0]))
    _store(x0, x2)
    _end_access(x2)
    let x3 = _access(.set, from: _subfield_view(x1, at: [0, 1]))
    _store(f, x3)
    _end_access(x3)
    return x1
  }

  /// Generates IR for jumping to projection continuation `c`.
  fileprivate mutating func _resume_continuation(_ c: Operand) {
    let x0 = _access(.let, from: _subfield_view(c, at: [0]))  // c.resumeFunction
    let x1 = _access(.let, from: _subfield_view(c, at: [1]))  // c.frame
    let x2 = _alloc_stack(.void)
    _emitApply(x0, to: [x1], writingResultTo: x2)
    _end_access(x1)
    _end_access(x0)
  }

  /// Emit code to call the projection ramp `r` with `arguments`, frame value type `t`,
  /// and continuation `continuation`.
  fileprivate mutating func _call_projection_ramp(
    _ r: FunctionReference, with arguments: [Operand], frameValueType t: AnyType,
    continuation: Operand
  ) {
    let x0 = _access(.let, from: continuation)
    let x1 = _access(.set, from: _alloc_stack(t))
    let x2 = _alloc_stack(.void)
    _emitApply(Operand.constant(r), to: arguments + [x0, x1], writingResultTo: x2)
    _end_access(x1)
    _end_access(x0)
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
  let regionSplitPositions: [SplitPosition]

  /// The number of regions in the caller scope.
  var regionsCount: Int {
    regionSplitPositions.count + 1
  }

  /// Returns the split instruction at the end of region `r`.
  internal func splitInstruction(endingRegion r: Int) -> InstructionID {
    precondition(r < regionsCount)
    return instructions[regionSplitPositions[r].splitPoint]
  }

  /// Returns the instructions in the "prefix" of region `r`, i.e. instructions without the epilogue.
  internal func prefixInstructions(region r: Int) -> Array<InstructionID>.SubSequence {
    precondition(r < regionsCount)
    let start = r == 0 ? instructions.startIndex : regionSplitPositions[r - 1].epilogueEnd
    let end = regionSplitPositions[r].splitPoint
    return instructions[start ..< end]
  }

  /// Returns the "epilogue" instructions of region `r`.
  internal func epilogueInstructions(region r: Int) -> Array<InstructionID>.SubSequence {
    precondition(r < regionsCount)
    let start = regionSplitPositions[r].splitPoint + 1
    let end = regionSplitPositions[r].epilogueEnd
    return instructions[start ..< end]
  }

  /// Returns all the instructions in region `r`.
  internal func instructions(region r: Int) -> Array<InstructionID>.SubSequence {
    precondition(r < regionsCount)
    let start = r == 0 ? instructions.startIndex : regionSplitPositions[r - 1].epilogueEnd
    let end = regionSplitPositions[r].epilogueEnd
    return instructions[start ..< end]
  }

  /// An instance describing projection caller `f`, with IR `source`, having `blocks` to be elaborated.
  ///
  /// - Precondition: all `blocks` are blocks of `source`.
  /// - Precondition: `blocks` is sorted by dominance order.
  init(_ f: Function.ID, source: Function, blocks: [Block.ID]) {
    self.id = f
    self.blocks = blocks
    let isSplitPoint = { (i: InstructionID) -> Bool in
      source[i] is Project || source[i] is EndProject
    }
    self.instructions = blocks.flatMap({ source.instructions(in: $0) })
    self.regionSplitPositions = source.split(instructions: self.instructions, where: isSplitPoint)
  }

}
