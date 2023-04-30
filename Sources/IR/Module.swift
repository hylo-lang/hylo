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
    try run({ closeBorrows(in: $0, diagnostics: &log) })
    try run({ ensureExclusivity(in: $0, diagnostics: &log) })
    try run({ normalizeObjectStates(in: $0, diagnostics: &log) })
  }

  /// Adds a global constant and returns its identity.
  mutating func addGlobal<C: Constant>(_ value: C) -> GlobalID {
    let id = globals.count
    globals.append(value)
    return id
  }

  /// Declares a function identified by `f` with type `t`.
  ///
  /// - Parameters:
  ///   - n: A human-readable name identifying the function.
  ///   - site: The site in the Val sources to which the function is attached.
  /// - Returns: `true` iff `f` wasn't already declared in `self`.
  @discardableResult
  mutating func declareFunction(
    identifiedBy f: Function.ID,
    typed t: LambdaType,
    named n: String? = nil,
    at site: SourceRange
  ) -> Bool {
    if functions[f] != nil { return false }

    let output = LoweredType.object(program.relations.canonical(t.output))
    var inputs: [ParameterType] = []
    appendCaptures(t.captures, passed: t.receiverEffect, to: &inputs)
    appendParameters(t.inputs, to: &inputs)

    functions[f] = Function(
      name: n ?? "",
      anchor: site.first(),
      linkage: .external,
      inputs: inputs,
      output: output,
      blocks: [])
    return true
  }

  /// Declares a subscript identified by `s` with type `t`.
  ///
  /// - Parameters:
  ///   - n: A human-readable name identifying the subscript.
  ///   - site: The site in the Val sources to which the subscript is attached.
  /// - Returns: `true` iff `s` wasn't already declared in `self`.
  @discardableResult
  mutating func declareSubscript(
    identifiedBy f: Function.ID,
    typed t: SubscriptImplType,
    named n: String?,
    at site: SourceRange
  ) -> Bool {
    if functions[f] != nil { return false }

    var inputs: [ParameterType] = []
    appendCaptures(t.captures, passed: t.receiverEffect, to: &inputs)
    appendParameters(t.inputs, to: &inputs)

    let output = program.relations.canonical(t.output)
    let callback = LambdaType(
      inputs: [.init(type: ^ParameterType(t.receiverEffect, output))],
      output: .void)
    inputs.append(.init(.let, ^callback))

    functions[f] = Function(
      name: n ?? "",
      anchor: site.first(),
      linkage: .external,
      inputs: inputs,
      output: .object(AnyType.void),
      blocks: [])
    return true
  }

  /// Appends to `inputs` the parameters corresponding to the given `captures` passed `effect`.
  private func appendCaptures(
    _ captures: [TupleType.Element],
    passed effect: AccessEffect,
    to inputs: inout [ParameterType]
  ) {
    inputs.reserveCapacity(captures.count)
    for c in captures {
      switch program.relations.canonical(c.type).base {
      case let p as RemoteType:
        precondition(p.access != .yielded, "cannot lower yielded parameter")
        inputs.append(ParameterType(p))
      case let p:
        precondition(effect != .yielded, "cannot lower yielded parameter")
        inputs.append(ParameterType(effect, ^p))
      }
    }
  }

  /// Appends `parameters` to `inputs`, ensuring that their types are canonical.
  private func appendParameters(
    _ parameters: [CallableTypeParameter],
    to inputs: inout [ParameterType]
  ) {
    inputs.reserveCapacity(parameters.count)
    for p in parameters {
      let t = ParameterType(program.relations.canonical(p.type))!
      precondition(t.access != .yielded, "cannot lower yielded parameter")
      inputs.append(t)
    }
  }

  /// Returns the identity of the Val IR function corresponding to `d`.
  mutating func getOrCreateFunction(lowering d: FunctionDecl.Typed) -> Function.ID {
    if let f = loweredFunctions[d.id] { return f }
    let f = Function.ID(d.id)
    let n = program.debugName(decl: d.id)

    switch d.type.base {
    case let declType as LambdaType:
      declareFunction(identifiedBy: f, typed: declType, named: n, at: d.site)
    case is MethodType:
      fatalError("not implemented")
    default:
      unreachable()
    }

    // Determine if the new function is the module's entry.
    if d.scope.kind == TranslationUnit.self, d.isPublic, d.identifier?.value == "main" {
      assert(entryFunctionID == nil)
      entryFunctionID = f
    }

    // Update the cache and return the ID of the newly created function.
    loweredFunctions[d.id] = f
    return f
  }

  /// Returns the identifier of the Val IR initializer corresponding to `d`.
  mutating func initializerDeclaration(lowering d: InitializerDecl.Typed) -> Function.ID {
    if let id = loweredFunctions[d.id] { return id }
    precondition(d.module == syntax)
    precondition(!d.isMemberwise)

    let declType = LambdaType(d.type)!
    let parameters = declType.inputs.map({ ParameterType($0.type)! })

    let f = Function.ID(initializer: d.id)
    assert(functions[f] == nil)
    functions[f] = Function(
      name: program.debugName(decl: d.id),
      anchor: d.introducer.site.first(),
      linkage: d.isPublic ? .external : .module,
      inputs: parameters,
      output: LoweredType.object(declType.output),
      blocks: [])

    // Update the cache and return the ID of the newly created function.
    loweredFunctions[d.id] = f
    return f
  }

  /// Appends a basic block to specified function and returns its identifier.
  @discardableResult
  mutating func appendBlock(
    taking parameters: [LoweredType] = [],
    to function: Function.ID
  ) -> Block.ID {
    let address = functions[function]!.appendBlock(taking: parameters)
    return Block.ID(function, address)
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
  mutating func replace<I: Instruction>(_ old: InstructionID, by new: I) -> [Operand] {
    precondition(self[old].types == new.types)
    removeUsesMadeBy(old)
    return insert(new) { (m, i) in
      m[old] = i
      return old
    }
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

}
