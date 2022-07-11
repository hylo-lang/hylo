// Takes a member from a record.
public struct TakeMemberInst: Inst {

  public let type: LoweredType

  /// The value of the record whose member is take.
  public let value: Operand

  /// A non-empty sequence of indices identifying a sub-object of `value`.
  public let path: [Int]

  public var operands: [Operand] { [value] }

}
