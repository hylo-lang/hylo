import VIL

/// A program counter.
struct ProgramCounter {

  /// The basic block containing the instruction being executed.
  let block: BasicBlock?

  /// The offset of the instruction in `block`.
  let offset: Int

  /// The instruction pointer by the program counter.
  var inst: Inst? {
    guard let block = self.block,
          offset < block.instructions.endIndex
    else { return nil }
    return block.instructions[offset]
  }

  /// Creates an undefined program counter.
  init() {
    self.block = nil
    self.offset = 0
  }

  /// Creates a program counter at the start of the given basic block.
  init(atStartOf block: BasicBlock) {
    self.block = block
    self.offset = 0
  }

  /// Creates a program counter pointing to the instruction at the specified offset in the given
  /// basic block.
  init?(block: BasicBlock, offset: Int) {
    guard offset < block.instructions.count else { return nil }
    self.block = block
    self.offset = offset
  }

  /// Returns an incremented program counter.
  func incremented() -> ProgramCounter? {
    guard let block = self.block else { return nil }
    return ProgramCounter(block: block, offset: offset + 1)
  }

}
