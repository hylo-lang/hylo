// Borrows an access on an object.
public struct Borrow: Inst {

  public let type: LoweredType

  /// The address of the object to borrow.
  public let source: Operand

  public var operands: [Operand] { [source] }


}
