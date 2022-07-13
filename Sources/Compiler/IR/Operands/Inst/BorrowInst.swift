// Borrows an access on an object or sub-object.
public struct BorrowInst: Inst {

  public var type: LoweredType

  /// The capability being borrowed.
  public var capability: ProjectionType.Capability

  /// The value of the root object on which an access is borrowed.
  public var value: Operand

  /// A sequence of indices identifying a sub-object of `value`.
  public var path: [Int]

  /// The binding in source program to which this instruction correspond, if any.
  public var binding: NodeID<VarDecl>? = nil

  public var range: SourceRange?

  public var operands: [Operand] { [value] }

}
