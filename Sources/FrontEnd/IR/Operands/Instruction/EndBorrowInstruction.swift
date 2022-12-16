/// Ends the lifetime of a borrow.
public struct EndBorrowInstruction: Instruction {

  /// The borrow whose lifetime is ended.
  public let borrow: Operand

  public let range: SourceRange?

  public var types: [LoweredType] { [] }

  public var operands: [Operand] { [borrow] }

  public var isTerminator: Bool { false }

  public func isWellFormed(in module: Module) -> Bool {
    true
  }

}
