import FrontEnd
import Utils

/// Provides functionality to write IR instructions for a function.
///
/// Maintains the constraints needed to have a well-formed IR.
public struct IRWriter {

  /// The ID of the function in which we are writing.
  public private(set) var function: Function.ID

  /// The IR that we are writing to.
  public private(set) var ir: Function!

  /// Calls `action` with an IR writer configured to modify `ir` for `function`.
  static func withInstance<T>(
    for function: Function.ID,
    with ir: inout Function,
    _ action: (inout IRWriter) throws -> T
  ) rethrows -> T {
    var instance = Self(function: function, ir: consume ir)
    defer {
      ir = instance.ir.release()
    }
    return try action(&instance)
  }

  /// Accesses the given block.
  public subscript(b: Block.ID) -> Block {
    _read { yield ir.blocks[b.address] }
    _modify { yield &ir[b.address] }
  }

  /// Accesses the given instruction.
  public subscript(i: InstructionID) -> Instruction {
    _read { yield ir.blocks[i.block].instructions[i.address] }
    _modify { yield &ir[i.block].instructions[i.address] }
  }

  /// Accesses the instruction denoted by `o` if it is `.register`; returns `nil` otherwise.
  public subscript(o: Operand) -> Instruction? {
    if case .register(let i) = o {
      return self[i]
    } else {
      return nil
    }
  }

  /// Appends to `f` an entry block that is in `scope`, returning its identifier.
  ///
  /// - Requires: `f` is declared in `self` and doesn't have an entry block.
  @discardableResult
  mutating func appendEntry<T: ScopeID>(in scope: T) -> Block.ID {
    assert(ir.blocks.isEmpty)

    // In functions, the last parameter of the entry denotes the function's return value.
    var parameters = ir.inputs.map({ IR.`Type`.address($0.type.bareType) })
    if !ir.isSubscript {
      parameters.append(.address(ir.output))
    }

    return appendBlock(in: scope, taking: parameters)
  }

  /// Appends to `f` a basic block that is in `scope` and accepts `parameters` to `f`, returning
  /// its identifier.
  ///
  /// - Requires: `f` is declared in `self`.
  @discardableResult
  mutating func appendBlock<T: ScopeID>(
    in scope: T,
    taking parameters: [IR.`Type`] = []
  ) -> Block.ID {
    let a = ir.appendBlock(in: scope, taking: parameters)
    return Block.ID(function, a)
  }

  /// Removes `block` and updates def-use chains.
  ///
  /// - Requires: No instruction in `block` is used by an instruction outside of `block`.
  @discardableResult
  mutating func removeBlock(_ block: Block.ID) -> Block {
    for i in self.instructions(in: block) {
      precondition(self.allUses(of: i).allSatisfy({ $0.user.block == block.address }))
      removeUsesMadeBy(i)
    }
    return ir.removeBlock(block.address)
  }

  /// Swaps `old` by `new`.
  ///
  /// `old` is removed and the def-use chains are updated so that the uses made by `old` are
  /// replaced by the uses made by `new` and all uses of `old` refer to `new`. After the call,
  /// `self[old] == new`.
  ///
  /// - Requires: `new` produces results with the same types as `old`.
  mutating func replace<I: Instruction>(_ old: InstructionID, with new: I) {
    precondition(self[old].result == new.result)
    removeUsesMadeBy(old)
    _ = insert(new) { (d, i) in
      d[old.block].instructions[old.address] = i
      return old
    }
  }

  /// Swaps all uses of `old` in `f` by `new` and updates the def-use chains.
  ///
  /// - Requires: `new` as the same type as `old`. `f` is in `self`.
  mutating func replaceUses(of old: Operand, with new: Operand, in f: Function.ID) {
    precondition(old != new)
    precondition(type(of: old) == type(of: new))

    guard var oldUses = ir.uses[old], !oldUses.isEmpty else { return }
    var newUses = ir.uses[new] ?? []

    var end = oldUses.count
    for i in oldUses.indices.reversed() where oldUses[i].user.function == f {
      let u = oldUses[i]
      self[u.user].replaceOperand(at: u.index, with: new)
      newUses.append(u)
      end -= 1
      oldUses.swapAt(i, end)
    }
    oldUses.removeSubrange(end...)

    ir.uses[old] = oldUses
    ir.uses[new] = newUses
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
    return insert(newInstruction) { (d, i) in
      InstructionID(block, d[block.address].instructions.prepend(newInstruction))
    }
  }

  /// Adds `newInstruction` at the end of `block` and returns its identity.
  @discardableResult
  private mutating func append(_ newInstruction: Instruction, to block: Block.ID) -> InstructionID {
    precondition(!(self[block].instructions.last is Terminator), "insertion after terminator")
    return insert(newInstruction) { (d, i) in
      InstructionID(block, d[block.address].instructions.append(newInstruction))
    }
  }

  /// Inserts `newInstruction` before `successor` and returns its identity.
  @discardableResult
  private mutating func insert(
    _ newInstruction: Instruction, before successor: InstructionID
  ) -> InstructionID {
    precondition(successor.function == function, "inserting into the wrong function")
    precondition(!(newInstruction is Terminator), "terminator must appear last in a block")
    return insert(newInstruction) { (d, i) in
      let address = d[successor.block].instructions
        .insert(newInstruction, before: successor.address)
      return InstructionID(successor.function, successor.block, address)
    }
  }

  /// Inserts `newInstruction` after `predecessor` and returns its identity.
  @discardableResult
  private mutating func insert(
    _ newInstruction: Instruction, after predecessor: InstructionID
  ) -> InstructionID {
    precondition(predecessor.function == function, "inserting into the wrong function")
    precondition(
      !(self.instruction(after: predecessor) is Terminator), "insertion after terminator")
    return insert(newInstruction) { (d, i) in
      let address = d[predecessor.block].instructions
        .insert(newInstruction, after: predecessor.address)
      return InstructionID(predecessor.function, predecessor.block, address)
    }
  }

  /// Inserts `newInstruction` with `impl` and returns its identity.
  private mutating func insert(
    _ newInstruction: Instruction, with impl: (inout Function, Instruction) -> InstructionID
  ) -> InstructionID {
    // Insert the instruction.
    let user = impl(&ir, newInstruction)

    // Update the def-use chains.
    for i in 0..<newInstruction.operands.count {
      ir.uses[newInstruction.operands[i], default: []].append(Use(user: user, index: i))
    }

    return user
  }

  /// Removes instruction `i` and updates def-use chains.
  ///
  /// - Requires: The result of `i` have no users.
  mutating func removeInstruction(_ i: InstructionID) {
    precondition(result(of: i).map(default: true, { ir.uses[$0, default: []].isEmpty }))
    removeUsesMadeBy(i)
    ir[i.block].instructions.remove(at: i.address)
  }

  /// Removes all instructions after `i` in its containing block and updates def-use chains.
  ///
  /// - Requires: Let `S` be the set of removed instructions, all users of a result of `j` in `S`
  ///   are also in `S`.
  mutating func removeAllInstructions(after i: InstructionID) {
    while let a = ir[i.block].instructions.lastAddress, a != i.address {
      removeInstruction(.init(i.function, i.block, a))
    }
  }

  /// Returns the uses of all the registers assigned by `i`.
  private func allUses(of i: InstructionID) -> [Use] {
    result(of: i).map(default: [], { ir.uses[$0, default: []] })
  }

  /// Removes `i` from the def-use chains of its operands.
  private mutating func removeUsesMadeBy(_ i: InstructionID) {
    for o in self[i].operands {
      ir.uses[o]?.removeAll(where: { $0.user == i })
    }
  }

  /// Returns the IDs of the blocks in `f`.
  ///
  /// The first element of the returned collection is the function's entry; other elements are in
  /// no particular order.
  func blocks(
    in f: Function.ID
  ) -> LazyMapSequence<Function.Blocks.Indices, Block.ID> {
    ir.blocks.indices.lazy.map({ .init(f, $0.address) })
  }

  /// Returns the IDs of the instructions in `b`, in order.
  func instructions(
    in b: Block.ID
  ) -> LazyMapSequence<Block.Instructions.Indices, InstructionID> {
    self[b].instructions.indices.lazy.map({ .init(b.function, b.address, $0.address) })
  }

  /// Returns the ID the instruction before `i`.
  func instruction(before i: InstructionID) -> InstructionID? {
    ir[i.block].instructions.address(before: i.address)
      .map({ InstructionID(i.function, i.block, $0) })
  }

  /// Returns the ID the instruction after `i`.
  func instruction(after i: InstructionID) -> InstructionID? {
    ir[i.block].instructions.address(after: i.address)
      .map({ InstructionID(i.function, i.block, $0) })
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
  func type(of operand: Operand) -> IR.`Type` {
    switch operand {
    case .register(let i):
      return ir[i.block][i.address].result!
    case .parameter(let b, let n):
      return ir[b.address].inputs[n]
    case .constant(let c):
      return c.type
    }
  }

}
