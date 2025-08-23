import FrontEnd
import Utils

extension Function {

  /// Appends to `self` a basic block in `scope` that accepts `parameters`, returning its address.
  ///
  /// The new block will become the function's entry if `self` contains no block before
  /// `appendBlock` is called.
  ///
  /// TODO: merge `appendBlock` and `appendEntry`
  mutating func appendBlock<T: ScopeID>(
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

    return appendBlock(in: scope, taking: parameters)
  }

  /// Removes `block` and updates def-use chains.
  ///
  /// - Requires: No instruction in `block` is used by an instruction outside of `block`.
  @discardableResult
  mutating func removeBlock(_ block: Block.ID) -> Block {
    for i in self.instructions(in: block) {
      precondition(self.allUses(of: i).allSatisfy({ blockForInstruction[$0.user] == block }))
      removeUsesMadeBy(i)
    }
    return blocks.remove(at: block.address)
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
    self[old] = new
    addUses(for: new, with: old)
  }

  /// Swaps all uses of `old` in `self` by `new` and updates the def-use chains.
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
    var i: InstructionID
    if self[block].first == nil {
      i = InstructionID(instructions.append(newInstruction))
    } else {
      precondition(!(newInstruction is Terminator), "terminator must appear last in a block")
      i = InstructionID(
        instructions.insert(newInstruction, before: self[block].first!.address))
    }
    blockForInstruction[i] = block
    self[block].setFirst(i)
    addUses(for: newInstruction, with: i)
    return i
  }

  /// Adds `newInstruction` at the end of `block` and returns its identity.
  @discardableResult
  private mutating func append(_ newInstruction: Instruction, to block: Block.ID) -> InstructionID {
    var i: InstructionID
    if self[block].last == nil {
      i = InstructionID(instructions.append(newInstruction))
    } else {
      precondition(!(self[terminator(of: block)!] is Terminator), "insertion after terminator")
      i = InstructionID(
        instructions.insert(newInstruction, after: self[block].last!.address))
    }
    blockForInstruction[i] = block
    self[block].setLast(i)
    addUses(for: newInstruction, with: i)
    return i
  }

  /// Inserts `newInstruction` before `successor` and returns its identity.
  @discardableResult
  private mutating func insert(
    _ newInstruction: Instruction, before successor: InstructionID
  ) -> InstructionID {
    precondition(!(newInstruction is Terminator), "terminator must appear last in a block")
    let b = blockForInstruction[successor]!
    let i = InstructionID(instructions.insert(newInstruction, before: successor.address))
    blockForInstruction[i] = b
    if self[b].first == successor {
      self[b].setFirst(i)
    }
    addUses(for: newInstruction, with: i)
    return i
  }

  /// Inserts `newInstruction` after `predecessor` and returns its identity.
  @discardableResult
  private mutating func insert(
    _ newInstruction: Instruction, after predecessor: InstructionID
  ) -> InstructionID {
    precondition(!(self[predecessor] is Terminator), "terminator must appear last in a block")
    let b = blockForInstruction[predecessor]!
    let i = InstructionID(instructions.insert(newInstruction, after: predecessor.address))
    blockForInstruction[i] = b
    if self[b].last == predecessor {
      self[b].setLast(i)
    }
    addUses(for: newInstruction, with: i)
    return i
  }

  /// Removes instruction `i` and updates def-use chains.
  ///
  /// - Requires: The result of `i` have no users.
  mutating func removeInstruction(_ i: InstructionID) {
    precondition(result(of: i).map(default: true, { uses[$0, default: []].isEmpty }))
    removeUsesMadeBy(i)
    uses[.register(i)] = nil
    removeFromBlock(i)
    instructions.remove(at: i.address)
  }

  /// Removes `i` from its corresponding block.
  mutating func removeFromBlock(_ i: InstructionID) {
    let b = blockForInstruction[i]!
    blockForInstruction[i] = nil
    if self[b].first == i && self[b].last == i {
      self[b].setFirst(nil)
      self[b].setLast(nil)
    } else if self[b].first == i {
      self[b].setFirst(instruction(after: i))
    } else if self[b].last == i {
      self[b].setLast(instruction(before: i))
    }
  }

  /// Removes all instructions after `i` in its containing block and updates def-use chains.
  ///
  /// - Requires: Let `S` be the set of removed instructions, all users of a result of `j` in `S`
  ///   are also in `S`.
  mutating func removeAllInstructions(after i: InstructionID) {
    while let a = self[blockForInstruction[i]!].last, a != i {
      removeInstruction(a)
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

}
