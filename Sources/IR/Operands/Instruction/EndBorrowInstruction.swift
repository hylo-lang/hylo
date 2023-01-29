import Core

/// Ends the lifetime of a borrow.
public struct EndBorrowInstruction: Instruction {

  /// The borrow whose lifetime is ended.
  public let borrow: Operand

  public let site: SourceRange

  init(borrow: Operand, site: SourceRange) {
    self.borrow = borrow
    self.site = site
  }

  public var types: [LoweredType] { [] }

  public var operands: [Operand] { [borrow] }

  public var isTerminator: Bool { false }

  public func isWellFormed(in module: Module) -> Bool {
    true
  }

}
