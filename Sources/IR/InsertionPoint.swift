/// Where an instruction should be inserted in a basic block.
enum InsertionPoint {

  /// The start of a basic block.
  case start(of: Block.AbsoluteID)

  /// The end of a basic block.
  case end(of: Block.AbsoluteID)

  /// Before another instruction.
  case before(AbsoluteInstructionID)

  /// After another instruction.
  case after(AbsoluteInstructionID)

  /// The block in which this insertion point falls.
  var block: Block.AbsoluteID {
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
