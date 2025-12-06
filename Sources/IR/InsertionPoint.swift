/// Where an instruction should be inserted in a basic block.
enum InsertionPoint: Sendable {

  /// The start of a basic block.
  case start(of: Block.ID)

  /// The end of a basic block.
  case end(of: Block.ID)

  /// Before another instruction.
  case before(InstructionID)

  /// After another instruction.
  case after(InstructionID)

  /// The block in which this insertion point falls, if a block is our anchor.
  var block: Block.ID? {
    switch self {
    case .start(let b):
      return b
    case .end(let b):
      return b
    default:
      return nil
    }
  }

}
