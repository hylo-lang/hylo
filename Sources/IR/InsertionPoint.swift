/// Where an instruction should be inserted in a basic block.
enum InsertionPoint {

  /// The end of a basic block.
  case at(endOf: Block.ID)

  /// Before another instruction.
  case before(InstructionID)

}
