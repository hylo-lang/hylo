// Takes a member from a record.
public struct TakeMemberInst: Inst {

  public var type: LoweredType

  /// The value of the record whose member is take.
  public var value: Operand

  /// A non-empty sequence of indices identifying a sub-object of `value`.
  public var path: [Int]

  public var range: SourceRange?

  public var operands: [Operand] { [value] }

}
