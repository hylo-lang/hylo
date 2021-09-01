/// An insertion pointer.
public struct InsertionPointer {

  public enum Position {

    case end

    case at(index: Int)

  }

  /// The name of the function in which new instructions are being inserted.
  public let funName: VILName

  /// The ID of the basic block in which new instructions are being inserted.
  public var blockID: BasicBlock.ID?

  /// The position at which instructions are being inserted.
  public var position: Position

  public init(
    funName: VILName,
    blockID: BasicBlock.ID? = nil,
    position: Position = .end
  ) {
    self.funName = funName
    self.blockID = blockID
    self.position = position
  }

}
