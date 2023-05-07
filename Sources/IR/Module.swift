import Core
import Foundation
import Utils

/// A module lowered to Val IR.
///
/// An IR module is notionally composed of a collection of functions, one of which may be
/// designated as its entry point (i.e., the `main` function of a Val program).
public struct Module {

  /// The identity of a global defined in a Val IR module.
  public typealias GlobalID = Int

  /// The program defining the functions in `self`.
  public let program: TypedProgram

  /// The module's syntax.
  public let syntax: ModuleDecl.Typed

  /// The def-use chains of the values in this module.
  public private(set) var uses: [Operand: [Use]] = [:]

  /// The globals in the module.
  public private(set) var globals: [any Constant] = []

  /// The functions in the module.
  public private(set) var functions: [Function.ID: Function] = [:]

  /// The ID of the module's entry function, if any.
  public private(set) var entryFunctionID: Function.ID?

  /// A map from function declaration its ID in the module.
  private var loweredFunctions = DeclProperty<Function.ID>()

  /// Creates an instance lowering `m` in `p`, reporting errors and warnings to
  /// `diagnostics`.
  ///
  /// - Requires: `m` is a valid ID in `p`.
  /// - Throws: `Diagnostics` if lowering fails.
  public init(
    lowering m: ModuleDecl.ID, in p: TypedProgram, diagnostics: inout DiagnosticSet
  ) throws {
    self.program = p
    self.syntax = program[m]

    var emitter = Emitter(program: program)
    emitter.emit(module: m, into: &self, diagnostics: &diagnostics)
    try diagnostics.throwOnError()
  }

  /// The module's name.
  public var name: String { syntax.baseName }

  /// Accesses the given function.
  public subscript(f: Function.ID) -> Function {
    _read { yield functions[f]! }
    _modify { yield &functions[f]! }
  }

  /// Accesses the given block.
  public subscript(b: Block.ID) -> Block {
    _read { yield functions[b.function]!.blocks[b.address] }
    _modify { yield &functions[b.function]![b.address] }
  }

  /// Accesses the given instruction.
  public subscript(i: InstructionID) -> Instruction {
    _read { yield functions[i.function]!.blocks[i.block].instructions[i.address] }
    _modify { yield &functions[i.function]![i.block].instructions[i.address] }
  }

  /// Returns the type of `operand`.
  public func type(of operand: Operand) -> LoweredType {
    switch operand {
    case .register(let instruction, let index):
      return functions[instruction.function]![instruction.block][instruction.address].types[index]

    case .parameter(let block, let index):
      return functions[block.function]![block.address].inputs[index]

    case .constant(let constant):
      return constant.type
    }
  }

  /// Returns the IDs of the blocks in `f`.
  ///
  /// The first element of the returned collection is the function's entry; other elements are in
  /// no particular order.
  public func blocks(
    in f: Function.ID
  ) -> LazyMapSequence<Function.Blocks.Indices, Block.ID> {
    self[f].blocks.indices.lazy.map({ .init(f, $0.address) })
  }

  /// Returns the IDs of the instructions in `b`, in order.
  public func instructions(
    in b: Block.ID
  ) -> LazyMapSequence<Block.Instructions.Indices, InstructionID> {
    self[b].instructions.indices.lazy.map({ .init(b.function, b.address, $0.address) })
  }

  /// Returns the ID the instruction before `i`.
  func instruction(before i: InstructionID) -> InstructionID? {
    functions[i.function]![i.block].instructions.address(before: i.address)
      .map({ InstructionID(i.function, i.block, $0) })
  }

  /// Returns the global identity of `block`'s terminator, if it exists.
  func terminator(of block: Block.ID) -> InstructionID? {
    if let a = functions[block.function]!.blocks[block.address].instructions.lastAddress {
      return InstructionID(block, a)
    } else {
      return nil
    }
  }

  /// Returns the global "past the end" position of `block`.
  func endIndex(of block: Block.ID) -> InstructionIndex {
    InstructionIndex(
      block, functions[block.function]!.blocks[block.address].instructions.endIndex)
  }

  /// Returns the registers asssigned by `i`.
  func results(of i: InstructionID) -> [Operand] {
    (0 ..< self[i].types.count).map({ .register(i, $0) })
  }

  /// Returns whether the IR in `self` is well-formed.
  ///
  /// Use this method as a sanity check to verify the module's invariants.
  public func isWellFormed() -> Bool {
    for f in functions.keys {
      if !isWellFormed(function: f) { return false }
    }
    return true
  }

  /// Returns whether `f` is well-formed.
  ///
  /// Use this method as a sanity check to verify the function's invariants.
  public func isWellFormed(function f: Function.ID) -> Bool {
    return true
  }

  /// Applies all mandatory passes in this module, accumulating diagnostics into `log` and throwing
  /// if a pass reports an error.
  public mutating func applyMandatoryPasses(
    reportingDiagnosticsInto log: inout DiagnosticSet
  ) throws {
    func run(_ pass: (Function.ID) -> Void) throws {
      for (k, f) in functions where f.entry != nil {
        pass(k)
      }
      try log.throwOnError()
    }

    try run({ removeDeadCode(in: $0, diagnostics: &log) })
    try run({ insertImplicitReturns(in: $0, diagnostics: &log) })
    try run({ reifyAccesses(in: $0, diagnostics: &log) })
    try run({ closeBorrows(in: $0, diagnostics: &log) })
    try run({ normalizeObjectStates(in: $0, diagnostics: &log) })
    try run({ ensureExclusivity(in: $0, diagnostics: &log) })
  }

  /// Adds a global constant and returns its identity.
  mutating func addGlobal<C: Constant>(_ value: C) -> GlobalID {
    let id = globals.count
    globals.append(value)
    return id
  }

  /// Returns the identity of the Val IR function corresponding to `d`.
  mutating func getOrCreateFunction(lowering d: FunctionDecl.Typed) -> Function.ID {
    if let f = loweredFunctions[d.id] { return f }
    let f = Function.ID(d.id)

    let output = program.relations.canonical((d.type.base as! CallableType).output)
    let inputs = loweredParameters(of: d.id)
    functions[f] = Function(
      isSubscript: false,
      name: program.debugName(decl: d.id),
      site: d.site,
      linkage: .external,
      inputs: inputs,
      output: output,
      blocks: [])

    // Determine if the new function is the module's entry.
    if d.scope.kind == TranslationUnit.self, d.isPublic, d.identifier?.value == "main" {
      assert(entryFunctionID == nil)
      entryFunctionID = f
    }

    loweredFunctions[d.id] = f
    return f
  }

  /// Returns the identity of the Val IR function implementing the `k` variant move-operator
  /// defined in conformance `c`.
  ///
  /// - Requires: `k` is either `.set` or `.inout`
  mutating func getOrCreateMoveOperator(_ k: AccessEffect, from c: Conformance) -> Function.ID {
    let d = program.ast.moveRequirement(k)
    switch c.implementations[d]! {
    case .concrete:
      fatalError("not implemented")
    case .synthetic(let t):
      let f = Function.ID(synthesized: d, for: t)
      declareSyntheticFunction(f, typed: LambdaType(t)!)
      return f
    }
  }

  /// Returns the identity of the Val IR function corresponding to `d`.
  mutating func getOrCreateSubscript(lowering d: SubscriptImpl.Typed) -> Function.ID {
    let f = Function.ID(d.id)
    if functions[f] != nil { return f }

    let output = program.relations.canonical(SubscriptImplType(d.type)!.output)
    let inputs = loweredParameters(of: d.id)
    functions[f] = Function(
      isSubscript: true,
      name: program.debugName(decl: d.id),
      site: d.site,
      linkage: .external,
      inputs: inputs,
      output: output,
      blocks: [])

    return f
  }

  /// Returns the identifier of the Val IR initializer corresponding to `d`.
  mutating func initializerDeclaration(lowering d: InitializerDecl.Typed) -> Function.ID {
    precondition(!d.isMemberwise)

    let f = Function.ID(initializer: d.id)
    if functions[f] != nil { return f }

    let inputs = loweredParameters(of: d.id)
    functions[f] = Function(
      isSubscript: false,
      name: program.debugName(decl: d.id),
      site: d.introducer.site,
      linkage: .external,
      inputs: inputs,
      output: .void,
      blocks: [])

    // Update the cache and return the ID of the newly created function.
    loweredFunctions[d.id] = f
    return f
  }

  /// Returns the lowered declarations of `d`'s parameters.
  private func loweredParameters(of d: FunctionDecl.ID) -> [Parameter] {
    let captures = LambdaType(program.declTypes[d]!)!.captures.lazy.map { (e) in
      program.relations.canonical(e.type)
    }
    var result: [Parameter] = zip(program.captures(of: d), captures).map({ (c, e) in
      .init(c, capturedAs: e)
    })
    result.append(contentsOf: program.ast[d].parameters.map(pairedWithLoweredType(parameter:)))
    return result
  }

  /// Returns the lowered declarations of `d`'s parameters.
  ///
  /// `d`'s receiver comes first and is followed by `d`'s formal parameters, from left to right.
  private func loweredParameters(of d: InitializerDecl.ID) -> [Parameter] {
    var result: [Parameter] = []
    result.append(pairedWithLoweredType(parameter: program.ast[d].receiver))
    result.append(contentsOf: program.ast[d].parameters.map(pairedWithLoweredType(parameter:)))
    return result
  }

  /// Returns the lowered declarations of `d`'s parameters.
  private func loweredParameters(of d: SubscriptImpl.ID) -> [Parameter] {
    let captures = SubscriptImplType(program.declTypes[d]!)!.captures.lazy.map { (e) in
      program.relations.canonical(e.type)
    }
    var result: [Parameter] = zip(program.captures(of: d), captures).map({ (c, e) in
      .init(c, capturedAs: e)
    })

    let bundle = SubscriptDecl.ID(program.declToScope[d]!)!
    if let p = program.ast[bundle].parameters {
      result.append(contentsOf: p.map(pairedWithLoweredType(parameter:)))
    }

    return result
  }

  /// Returns `d`, which declares a parameter, paired with its lowered type.
  private func pairedWithLoweredType(parameter d: ParameterDecl.ID) -> Parameter {
    let t = program.relations.canonical(program.declTypes[d]!)
    return .init(decl: AnyDeclID(d), type: ParameterType(t)!)
  }

  /// Declares a synthetic function identified by `f` with type `t`.
  mutating func declareSyntheticFunction(_ f: Function.ID, typed t: LambdaType) {
    if functions[f] != nil { return }

    let output = program.relations.canonical(t.output)
    var inputs: [Parameter] = []
    appendCaptures(t.captures, passed: t.receiverEffect, to: &inputs)
    appendParameters(t.inputs, to: &inputs)

    functions[f] = Function(
      isSubscript: false,
      name: "",
      site: .empty(at: syntax.site.first()),
      linkage: .external,
      inputs: inputs,
      output: output,
      blocks: [])
  }

  /// Appends to `inputs` the parameters corresponding to the given `captures` passed `effect`.
  private func appendCaptures(
    _ captures: [TupleType.Element],
    passed effect: AccessEffect,
    to inputs: inout [Parameter]
  ) {
    inputs.reserveCapacity(captures.count)
    for c in captures {
      switch program.relations.canonical(c.type).base {
      case let p as RemoteType:
        precondition(p.access != .yielded, "cannot lower yielded parameter")
        inputs.append(.init(decl: nil, type: ParameterType(p)))
      case let p:
        precondition(effect != .yielded, "cannot lower yielded parameter")
        inputs.append(.init(decl: nil, type: ParameterType(effect, ^p)))
      }
    }
  }

  /// Appends `parameters` to `inputs`, ensuring that their types are canonical.
  private func appendParameters(
    _ parameters: [CallableTypeParameter],
    to inputs: inout [Parameter]
  ) {
    inputs.reserveCapacity(parameters.count)
    for p in parameters {
      let t = ParameterType(program.relations.canonical(p.type))!
      precondition(t.access != .yielded, "cannot lower yielded parameter")
      inputs.append(.init(decl: nil, type: t))
    }
  }

  /// Returns the entry of `f`.
  ///
  /// - Requires: `f` is declared in `self`.
  func entry(of f: Function.ID) -> Block.ID? {
    functions[f]!.entry.map({ Block.ID(f, $0) })
  }

  /// Appends an entry block to `f` and returns its identifier.
  ///
  /// - Requires: `f` is declared in `self` and doesn't have an entry block.
  @discardableResult
  mutating func appendEntry(to f: Function.ID) -> Block.ID {
    assert(functions[f]!.blocks.isEmpty)
    return appendBlock(taking: functions[f]!.inputs.map({ .address($0.type.bareType) }), to: f)
  }

  /// Appends a basic block taking `parameters` to `f` and returns its identifier.
  ///
  /// - Requires: `f` is declared in `self`.
  @discardableResult
  mutating func appendBlock(
    taking parameters: [LoweredType] = [],
    to f: Function.ID
  ) -> Block.ID {
    let a = functions[f]!.appendBlock(taking: parameters)
    return Block.ID(f, a)
  }

  /// Removes `block` and updates def-use chains.
  ///
  /// - Requires: No instruction in `block` is used by an instruction outside of `block`.
  @discardableResult
  mutating func removeBlock(_ block: Block.ID) -> Block {
    for i in instructions(in: block) {
      precondition(allUses(of: i).allSatisfy({ $0.user.block == block.address }))
      removeUsesMadeBy(i)
    }
    return functions[block.function]!.removeBlock(block.address)
  }

  /// Swaps `old` by `new` and returns the identities of the latter's return values.
  ///
  /// `old` is removed from to module and the def-use chains are updated.
  ///
  /// - Requires: `new` produces results with the same types as `old`.
  @discardableResult
  mutating func replace<I: Instruction>(_ old: InstructionID, with new: I) -> [Operand] {
    precondition(self[old].types == new.types)
    removeUsesMadeBy(old)
    return insert(new) { (m, i) in
      m[old] = i
      return old
    }
  }

  /// Swaps all uses of `old` in `f` by `new` and updates the def-use chains.
  ///
  /// - Requires: `new` as the same type as `old`. `f` is in `self`.
  mutating func replaceUses(of old: Operand, with new: Operand, in f: Function.ID) {
    precondition(old != new)
    precondition(type(of: old) == type(of: new))

    guard var oldUses = uses[old], !oldUses.isEmpty else { return }
    var newUses = uses[new] ?? []

    var end = oldUses.count
    for i in oldUses.indices.reversed() where oldUses[i].user.function == f {
      let u = oldUses[i]
      self[u.user].replaceOperand(at: u.index, with: new)
      newUses.append(u)
      end -= 1
      oldUses.swapAt(i, end)
    }
    oldUses.removeSubrange(end...)

    uses[old] = oldUses
    uses[new] = newUses
  }

  /// Adds `newInstruction` at the end of `block` and returns the identities of its return values.
  @discardableResult
  mutating func append<I: Instruction>(
    _ newInstruction: I,
    to block: Block.ID
  ) -> [Operand] {
    insert(
      newInstruction,
      with: { (m, i) in
        InstructionID(block, m[block].instructions.append(newInstruction))
      })
  }

  /// Inserts `newInstruction` at `position` and returns the identities of its return values.
  ///
  /// The instruction is inserted before the instruction currently at `position`. You can pass a
  /// "past the end" position to append at the end of a block.
  @discardableResult
  mutating func insert<I: Instruction>(
    _ newInstruction: I,
    at position: InstructionIndex
  ) -> [Operand] {
    insert(newInstruction) { (m, i) in
      let address = m.functions[position.function]![position.block].instructions
        .insert(newInstruction, at: position.index)
      return InstructionID(position.function, position.block, address)
    }
  }

  /// Inserts `newInstruction` before the instruction identified by `successor` and returns the
  /// identities of its results.
  @discardableResult
  mutating func insert<I: Instruction>(
    _ newInstruction: I,
    before successor: InstructionID
  ) -> [Operand] {
    insert(newInstruction) { (m, i) in
      let address = m.functions[successor.function]![successor.block].instructions
        .insert(newInstruction, before: successor.address)
      return InstructionID(successor.function, successor.block, address)
    }
  }

  /// Inserts `newInstruction` after the instruction identified by `predecessor` and returns the
  /// identities of its results.
  @discardableResult
  mutating func insert<I: Instruction>(
    _ newInstruction: I,
    after predecessor: InstructionID
  ) -> [Operand] {
    insert(newInstruction) { (m, i) in
      let address = m.functions[predecessor.function]![predecessor.block].instructions
        .insert(newInstruction, after: predecessor.address)
      return InstructionID(predecessor.function, predecessor.block, address)
    }
  }

  /// Inserts `newInstruction` with `impl` and returns the identities of its return values.
  private mutating func insert<I: Instruction>(
    _ newInstruction: I,
    with impl: (inout Self, I) -> InstructionID
  ) -> [Operand] {
    // Insert the instruction.
    let user = impl(&self, newInstruction)

    // Update the def-use chains.
    for i in 0 ..< newInstruction.operands.count {
      uses[newInstruction.operands[i], default: []].append(Use(user: user, index: i))
    }

    return results(of: user)
  }

  /// Removes instruction `i` and updates def-use chains.
  ///
  /// - Requires: The results of `i` have no users.
  mutating func removeInstruction(_ i: InstructionID) {
    precondition(results(of: i).allSatisfy({ uses[$0, default: []].isEmpty }))
    removeUsesMadeBy(i)
    self[i.function][i.block].instructions.remove(at: i.address)
  }

  /// Removes all instructions after `i` in its containing block and updates def-use chains.
  ///
  /// - Requires: Let `S` be the set of removed instructions, all users of a result of `j` in `S`
  ///   are also in `S`.
  mutating func removeAllInstructionsAfter(_ i: InstructionID) {
    while let a = self[i.function][i.block].instructions.lastAddress, a != i.address {
      removeInstruction(.init(i.function, i.block, a))
    }
  }

  /// Returns the uses of all the registers assigned by `i`.
  private func allUses(of i: InstructionID) -> FlattenSequence<[[Use]]> {
    results(of: i).compactMap({ uses[$0] }).joined()
  }

  /// Removes `i` from the def-use chains of its operands.
  private mutating func removeUsesMadeBy(_ i: InstructionID) {
    for o in self[i].operands {
      uses[o]?.removeAll(where: { $0.user == i })
    }
  }

  /// Returns the operands from which the address denoted by `a` derives.
  ///
  /// The (static) provenances of an address denote the original operands from which it derives.
  /// They form a set because an address computed by a projection depends on that projection's
  /// arguments and because an address defined as a parameter of a basic block with multiple
  /// predecessors depends on that bock's arguments.
  func provenances(_ a: Operand) -> Set<Operand> {
    // TODO: Block arguments
    guard case .register(let i, _) = a else { return [a] }

    switch self[i] {
    case let s as BorrowInstruction:
      return provenances(s.location)
    case let s as ElementAddrInstruction:
      return provenances(s.base)
    case let s as ProjectInstruction:
      return s.operands.reduce(
        into: [],
        { (p, o) in
          if type(of: o).isAddress { p.formUnion(provenances(o)) }
        })
    case let s as WrapAddrInstruction:
      return provenances(s.witness)
    default:
      return [a]
    }
  }

  /// Returns `true` if `o` is sinkable in `f`.
  ///
  /// - Requires: `o` is defined in `f`.
  func isSinkable(_ o: Operand, in f: Function.ID) -> Bool {
    let e = entry(of: f)!
    return provenances(o).allSatisfy { (p) -> Bool in
      if case .parameter(e, let i) = p {
        return self.functions[f]!.inputs[i].type.access == .sink
      } else {
        return true
      }
    }
  }

}
