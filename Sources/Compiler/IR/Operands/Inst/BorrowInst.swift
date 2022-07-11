// Borrows an access on an object or sub-object.
public struct BorrowInst: Inst {

  public let type: LoweredType

  /// The capability being borrowed.
  public let capability: ProjectionType.Capability

  /// The value of the root object on which an access is borrowed.
  public let value: Operand

  /// A sequence of indices identifying a sub-object of `value`.
  public let path: [Int]

  public var operands: [Operand] { [value] }

}
