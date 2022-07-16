/// Ends the lifetime of a borrow.
public struct EndBorrowInst: Inst {

  /// The borrow whose lifetime is ended.
  public var borrow: Operand

  public var range: SourceRange?

  public var types: [LoweredType] { [] }

  public var operands: [Operand] { [borrow] }

  public func check(in module: Module) -> Bool {
    true
  }

}
