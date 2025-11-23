import FrontEnd
import Utils

/// A collection of basic blocks representing a lowered function.
public struct Function: Sendable {

  /// A collection of blocks with stable identities.
  public typealias Blocks = DoublyLinkedList<Block>

  /// `true` iff the function implements a subscript.
  public let isSubscript: Bool

  /// The site in the source code to which the function corresponds..
  public let site: SourceRange

  /// The linkage of the function.
  public let linkage: Linkage

  /// The generic (a.k.a., compile-time) parameters of the function.
  public let genericParameters: [GenericParameterDecl.ID]

  /// The run-time parameters of the function.
  public let inputs: [Parameter]

  /// The type of the function's output.
  public let output: AnyType

  /// The basic blocks of the control flow graph.
  public var blocks: Blocks

  /// The def-use chains of the values in this module.
  public var uses: [Operand: [Use]] = [:]

  /// Accesses the basic block at `address`.
  ///
  /// - Requires: `address` must be a valid address in `self`.
  public subscript(_ address: Blocks.Address) -> Block {
    get { blocks[address] }
    _modify { yield &blocks[address] }
  }

  /// Accesses the block identified by `b`.
  public subscript(b: Block.ID) -> Block {
    _read { yield blocks[b.address] }
    _modify { yield &blocks[b.address] }
  }

  /// Accesses the instruction identified by `i`.
  public subscript(i: InstructionID) -> Instruction {
    _read { yield blocks[i.block].instructions[i.address] }
    _modify { yield &blocks[i.block].instructions[i.address] }
  }

  /// Accesses the instruction denoted by `o` if it is `.register`; returns `nil` otherwise.
  public subscript(register o: Operand) -> Instruction? {
    if case .register(let i) = o {
      return self[i]
    } else {
      return nil
    }
  }

  /// The entry of the function.
  public var entry: Block.ID? { blocks.firstAddress.map(Block.ID.init) }

  /// `true` iff the function takes generic parameters.
  public var isGeneric: Bool {
    !genericParameters.isEmpty
  }

  /// Returns the identity of the return value.
  public var returnValue: Operand? {
    if !isSubscript, let e = entry {
      return Operand.parameter(e, inputs.count)
    } else {
      return nil
    }
  }

  /// The IDs of all basic blocks in the CFG, beginning with the entry point block.
  ///
  /// The first element of the returned collection is the function's entry; other elements are in
  /// no particular order.
  public var blockIDs: some RandomAccessCollection<Block.ID> {
    blocks.indices.lazy.map({ .init($0.address) })
  }

  /// The IDs of all instructions.
  public var instructions: some Collection<InstructionID> {
    blocks.indices.lazy.flatMap({ instructions(in: Block.ID($0.address)) })
  }

  /// Returns the control flow graph of `self`.
  func cfg() -> ControlFlowGraph {
    var result = ControlFlowGraph()
    for source in blocks.indices {
      guard let s = blocks[source.address].instructions.last as? Terminator else { continue }
      for target in s.successors {
        result.define(source.address, predecessorOf: target.address)
      }
    }

    return result
  }


  /// Appends a basic block in `scope` accepting `parameters`, returning its address.
  ///
  /// The first block appended becomes the entry point block.
  ///
  /// TODO: merge `appendBlock` and `appendEntry`
  mutating func append<T: ScopeID>(
    in scope: T, taking parameters: [IR.`Type`] = []
  ) -> Block.ID {
    Block.ID(blocks.append(Block(scope: AnyScopeID(scope), inputs: parameters)))
  }

  /// Appends to `self` an entry block that is in `scope`, returning its identifier.
  @discardableResult
  mutating func appendEntry<T: ScopeID>(in scope: T) -> Block.ID {
    assert(blocks.isEmpty)

    // In functions, the last parameter of the entry denotes the function's return value.
    var parameters = inputs.map({ `Type`.address($0.type.bareType) })
    if !isSubscript {
      parameters.append(.address(output))
    }

    return append(in: scope, taking: parameters)
  }

  /// Removes `block` and updates def-use chains.
  ///
  /// - Requires: No instruction in `block` is used by an instruction outside of `block`.
  @discardableResult
  mutating func remove(_ block: Block.ID) -> Block {
    for i in self.instructions(in: block) {
      precondition(self.allUses(of: i).allSatisfy({ $0.user.block == block.address }))
      removeUsesMadeBy(i)
    }
    return blocks.remove(at: block.address)
  }

  /// Replaces `old` with `new`.
  ///
  /// The def-use chains are updated so that the uses formerly made by `old` are
  /// replaced by the uses made by `new` and all former uses of `old` refer to `new`. After the call,
  /// `self[old] == new`.
  ///
  /// - Requires: `new` produces results with the same types as `old`.
  mutating func replace<I: Instruction>(_ old: InstructionID, with new: I) {
    precondition(self[old].result == new.result)
    removeUsesMadeBy(old)
    self[old] = new
    addUses(for: new, with: old)
  }

  /// Replaces all uses of `old` with `new` and updates the def-use chains.
  ///
  /// The def-use chains are updated so that the uses formerly made by `old` are
  /// replaced by the uses made by `new` and all former uses of `old` refer to `new`.
  ///
  /// - Requires: `new` as the same type as `old`.
  mutating func replaceUses(of old: Operand, with new: Operand) {
    precondition(old != new)
    precondition(type(of: old) == type(of: new))

    guard var oldUses = uses[old], !oldUses.isEmpty else { return }
    var newUses = uses[new] ?? []

    var end = oldUses.count
    for i in oldUses.indices.reversed() {
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

  /// Inserts `newInstruction` at `boundary` and returns its identity.
  @discardableResult
  mutating func insert(
    _ newInstruction: Instruction, at boundary: InsertionPoint
  ) -> InstructionID {
    switch boundary {
    case .start(let b):
      return prepend(newInstruction, to: b)
    case .end(let b):
      return append(newInstruction, to: b)
    case .before(let i):
      return insert(newInstruction, before: i)
    case .after(let i):
      return insert(newInstruction, after: i)
    }
  }

  /// Adds `newInstruction` at the start of `block` and returns its identity.
  @discardableResult
  private mutating func prepend(_ newInstruction: Instruction, to block: Block.ID) -> InstructionID
  {
    precondition(!(newInstruction is Terminator), "terminator must appear last in a block")
    let i = InstructionID(block, self[block.address].instructions.prepend(newInstruction))
    addUses(for: newInstruction, with: i)
    return i
  }

  /// Adds `newInstruction` at the end of `block` and returns its identity.
  @discardableResult
  private mutating func append(_ newInstruction: Instruction, to block: Block.ID) -> InstructionID {
    precondition(!(self[block].instructions.last is Terminator), "insertion after terminator")
    let i = InstructionID(block, self[block.address].instructions.append(newInstruction))
    addUses(for: newInstruction, with: i)
    return i
  }

  /// Inserts `newInstruction` before `successor` and returns its identity.
  @discardableResult
  private mutating func insert(
    _ newInstruction: Instruction, before successor: InstructionID
  ) -> InstructionID {
    precondition(!(newInstruction is Terminator), "terminator must appear last in a block")
    let i = InstructionID(
      successor.block,
      self[successor.block].instructions.insert(newInstruction, before: successor.address))
    addUses(for: newInstruction, with: i)
    return i
  }

  /// Inserts `newInstruction` after `predecessor` and returns its identity.
  @discardableResult
  private mutating func insert(
    _ newInstruction: Instruction, after predecessor: InstructionID
  ) -> InstructionID {
    precondition(
      !(self.instruction(after: predecessor) is Terminator), "insertion after terminator")
    let i = InstructionID(
      predecessor.block,
      self[predecessor.block].instructions.insert(newInstruction, after: predecessor.address))
    addUses(for: newInstruction, with: i)
    return i
  }

  /// Removes instruction `i` and updates def-use chains.
  ///
  /// - Requires: The result of `i` have no users.
  mutating func remove(_ i: InstructionID) {
    precondition(result(of: i).map(default: true, { uses[$0, default: []].isEmpty }))
    removeUsesMadeBy(i)
    self[i.block].instructions.remove(at: i.address)
  }

  /// Removes all instructions after `i` in its containing block and updates def-use chains.
  ///
  /// - Requires: Let `S` be the set of removed instructions, all users of a result of `j` in `S`
  ///   are also in `S`.
  mutating func removeAllInstructions(after i: InstructionID) {
    while let a = self[i.block].instructions.lastAddress, a != i.address {
      remove(.init(i.block, a))
    }
  }

  /// Keep track of uses for `instruction` identified by `id`.
  private mutating func addUses(for instruction: Instruction, with id: InstructionID) {
    for i in 0..<instruction.operands.count {
      uses[instruction.operands[i], default: []].append(Use(user: id, index: i))
    }
  }

  /// Removes `i` from the def-use chains of its operands.
  private mutating func removeUsesMadeBy(_ i: InstructionID) {
    for o in self[i].operands {
      uses[o]?.removeAll(where: { $0.user == i })
    }
  }


  /// Returns the IDs of the instructions in `b`, in order.
  public func instructions(
    in b: Block.ID
  ) -> LazyMapSequence<Block.Instructions.Indices, InstructionID> {
    self[b].instructions.indices.lazy.map({ .init(b.address, $0.address) })
  }

  /// Returns the ID of the first instruction in `b`, if any.
  public func firstInstruction(in b: Block.ID) -> InstructionID? {
    self[b].instructions.firstAddress.map({ InstructionID(b.address, $0) })
  }

  /// Returns the ID the instruction before `i`.
  func instruction(before i: InstructionID) -> InstructionID? {
    self[i.block].instructions.address(before: i.address)
      .map({ InstructionID(i.block, $0) })
  }

  /// Returns the ID the instruction after `i`.
  func instruction(after i: InstructionID) -> InstructionID? {
    self[i.block].instructions.address(after: i.address)
      .map({ InstructionID(i.block, $0) })
  }

  /// Returns the scope in which `i` is used.
  public func scope(containing i: InstructionID) -> AnyScopeID {
    self[i.block].scope
  }

  /// Returns `true` iff `lhs` is sequenced before `rhs` in the block of `lhs`.
  /// Returns `false` if the two instructions are not in the same block.
  public func precedes(_ lhs: InstructionID, _ rhs: InstructionID) -> Bool {
    return lhs.address.precedes(rhs.address, in: self[lhs.block].instructions)
  }

  /// Returns `true` iff `lhs` is sequenced before `rhs`.
  func dominates(_ lhs: InstructionID, _ rhs: InstructionID) -> Bool {
    // Fast path: both instructions are in the same block.
    if lhs.block == rhs.block {
      let sequence = self[lhs.block].instructions
      return lhs.address.precedes(rhs.address, in: sequence)
    }

    // Slow path: use the dominator tree.
    let d = DominatorTree(function: self, cfg: cfg())
    return d.dominates(lhs.block, rhs.block)
  }

  /// Returns the global identity of `block`'s terminator, if it exists.
  func terminator(of block: Block.ID) -> InstructionID? {
    if let a = self[block].instructions.lastAddress {
      return InstructionID(block, a)
    } else {
      return nil
    }
  }

  /// Returns the register assigned by `i`, if any.
  func result(of i: InstructionID) -> Operand? {
    if self[i].result != nil {
      return .register(i)
    } else {
      return nil
    }
  }

  /// Returns the type of `operand`.
  public func type(of operand: Operand) -> IR.`Type` {
    switch operand {
    case .register(let i):
      return blocks[i.block][i.address].result!
    case .parameter(let b, let n):
      return blocks[b.address].inputs[n]
    case .constant(let c):
      return c.type
    }
  }

  /// Returns `true` iff `p` cannot be used to modify or update a value.
  public func isBoundImmutably(_ p: Operand) -> Bool {
    switch p {
    case .parameter(let e, let i):
      return (entry == e) && (passingConvention(parameter: i) == .let)
    case .constant:
      return false
    case .register(let i):
      return isBoundImmutably(register: i)
    }
  }

  /// Returns `true` iff the result of `i` cannot be used to modify or update a value.
  public func isBoundImmutably(register i: InstructionID) -> Bool {
    switch self[i] {
    case is AllocStack:
      return false
    case let s as AdvancedByBytes:
      return isBoundImmutably(s.base)
    case let s as AdvancedByStrides:
      return isBoundImmutably(s.base)
    case let s as Access:
      return isBoundImmutably(s.source)
    case let s as OpenCapture:
      return s.isAccess(.let)
    case is OpenUnion:
      return false
    case let s as PointerToAddress:
      return s.isAccess(.let)
    case let s as Project:
      return s.projection.access == .let
    case let s as SubfieldView:
      return isBoundImmutably(s.recordAddress)
    case let s as WrapExistentialAddr:
      return isBoundImmutably(s.witness)
    default:
      return true
    }
  }

  /// If `p` is a function parameter, returns its passing convention. Otherwise, returns `nil`.
  public func passingConvention(of p: Operand) -> AccessEffect? {
    if case .parameter(let e, let i) = p, entry == e {
      return passingConvention(parameter: i)
    } else {
      return nil
    }
  }

  /// Returns the passing convention of the `i`-th parameter of `self`.
  public func passingConvention(parameter i: Int) -> AccessEffect {
    // The last parameter of a function denotes its return value.
    return (i == inputs.count) ? .set : inputs[i].type.access
  }

  /// Returns the uses of all the registers assigned by `i`.
  func allUses(of i: InstructionID) -> [Use] {
    result(of: i).map(default: [], { uses[$0, default: []] })
  }

  /// Returns the operands from which the address denoted by `a` derives.
  ///
  /// The (static) provenances of an address denote the original operands from which it derives.
  /// They form a set because an address computed by a projection depends on that projection's
  /// arguments and because an address defined as a parameter of a basic block with multiple
  /// predecessors depends on that bock's arguments.
  func provenances(_ a: Operand) -> Set<Operand> {
    // TODO: Block arguments
    guard let i = a.instruction else { return [a] }

    switch self[i] {
    case let s as AdvancedByBytes:
      return provenances(s.base)
    case let s as Access:
      return provenances(s.source)
    case let s as Project:
      return s.operands.reduce(into: []) { (p, o) in
        if type(of: o).isAddress { p.formUnion(provenances(o)) }
      }
    case let s as SubfieldView:
      return provenances(s.recordAddress)
    case let s as WrapExistentialAddr:
      return provenances(s.witness)
    default:
      return [a]
    }
  }

  /// Returns `true` if `o` can be sunken.
  func isSink(_ o: Operand) -> Bool {
    provenances(o).allSatisfy { (p) -> Bool in
      switch p {
      case .parameter(let e, let i):
        if entry == e {
          return inputs[i].type.access == .sink
        } else {
          return false
        }

      case .register(let i):
        switch self[i] {
        case let s as ProjectBundle:
          return s.capabilities.contains(.sink)
        case let s as Project:
          return s.projection.access == .sink
        default:
          return true
        }

      default:
        return true
      }
    }
  }

  /// Returns `true` iff `o` is an `access [set]` instruction.
  func isBorrowSet(_ o: Operand) -> Bool {
    guard
      let i = o.instruction,
      let s = self[i] as? Access
    else { return false }
    return s.capabilities == .set
  }

}
