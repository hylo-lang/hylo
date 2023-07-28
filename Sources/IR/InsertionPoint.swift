/// Where an instruction should be inserted in a basic block.
enum InsertionPoint {

  /// The end of a basic block.
  case end(of: Block.ID)

  /// Before another instruction.
  case before(InstructionID)

  /// The block in which this insertion point falls.
  var block: Block.ID {
    switch self {
    case .end(let b):
      return b
    case .before(let i):
      return .init(i.function, i.block)
    }
  }

}
