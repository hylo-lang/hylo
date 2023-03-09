import Core

/// Ends the lifetime of a borrow.
public struct EndBorrowInstruction: Instruction {

  /// The borrow whose lifetime is ended.
  public let borrow: Operand

  public let site: SourceRange

  /// Creates an instance with the given properties.
  fileprivate init(borrow: Operand, site: SourceRange) {
    self.borrow = borrow
    self.site = site
  }

  public var types: [LoweredType] { [] }

  public var operands: [Operand] { [borrow] }

}

extension Module {

  /// Creates an `end_borrow` anchored at `anchor` that ends a borrow previously created by
  /// `borrow`.
  ///
  /// - Parameters:
  ///   - borrow: The borrow to end. Must be the result of `borrow`.
  func makeEndBorrow(
    _ borrow: Operand,
    anchoredAt anchor: SourceRange
  ) -> EndBorrowInstruction {
    precondition(borrow.instruction.map({ self[$0] is BorrowInstruction }) ?? false)
    return EndBorrowInstruction(borrow: borrow, site: anchor)
  }

}
