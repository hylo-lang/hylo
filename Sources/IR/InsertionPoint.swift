/// Where an instruction should be inserted in a basic block.
enum InsertionPoint {

  /// The start of a basic block.
  case start(of: Block.ID)

  /// The end of a basic block.
  case end(of: Block.ID)

  /// Before another instruction.
  case before(InstructionID)

  /// After another instruction.
  case after(InstructionID)

  /// The block in which this insertion point falls.
  var block: Block.ID {
    switch self {
    case .start(let b):
      return b
    case .end(let b):
      return b
    case .before(let i):
      return .init(i.function, i.block)
    case .after(let i):
      return .init(i.function, i.block)
    }
  }

}
