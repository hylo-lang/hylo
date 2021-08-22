import VIL

/// A program counter.
struct ProgramCounter {

  /// The function containing the instruction being executed.
  let function: Function?

  /// The ID of the basic block containing the instruction being executed.
  let blockID: BasicBlock.ID?

  /// The offset of the instruction in `block`.
  let offset: Int

  /// The instruction pointer by the program counter.
  var inst: Inst? {
    guard let blockID = self.blockID else { return nil }
    guard offset < function!.blocks[blockID]!.instructions.endIndex else { return nil }
    return function!.blocks[blockID]!.instructions[offset]
  }

  /// Creates an undefined program counter.
  private init() {
    self.function = nil
    self.blockID = nil
    self.offset = 0
  }

  /// Creates a program counter at the start of the given basic block.
  init(atStartOf blockID: BasicBlock.ID, in function: Function) {
    precondition(function.blocks[blockID] != nil, "specified block is not in the function")
    self.function = function
    self.blockID = blockID
    self.offset = 0
  }

  /// Creates a program counter pointing to the instruction at the specified offset in the given
  /// basic block.
  init?(blockID: BasicBlock.ID, in function: Function, offsetBy offset: Int) {
    precondition(function.blocks[blockID] != nil, "specified block is not in the function")
    guard offset < function.blocks[blockID]!.instructions.count else { return nil }

    self.function = function
    self.blockID = blockID
    self.offset = offset
  }

  /// Returns an incremented program counter.
  func incremented() -> ProgramCounter? {
    guard let blockID = self.blockID else { return nil }
    return ProgramCounter(blockID: blockID, in: function!, offsetBy: offset + 1)
  }

  /// An undefined program counter.
  static var undefined: ProgramCounter { ProgramCounter() }

}
