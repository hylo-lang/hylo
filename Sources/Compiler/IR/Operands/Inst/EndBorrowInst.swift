/// Ends the lifetime of a borrow.
public struct EndBorrowInst: Inst {

  /// The borrow whose lifetime is ended.
  public var borrow: Operand

  public var range: SourceRange?

  public var type: LoweredType { .object(.unit) }

  public var operands: [Operand] { [borrow] }

}
