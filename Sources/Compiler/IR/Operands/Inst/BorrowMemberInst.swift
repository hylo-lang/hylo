// Borrows an access on a record member.
public struct BorrowMemberInst: Inst {

  public let type: LoweredType

  /// The value of the record whose member address is computed.
  public let value: Operand

  /// The indices of the index member.
  ///
  /// - Requires: This array cannot be empty.
  public let path: [Int]

  public var operands: [Operand] { [value] }

}
