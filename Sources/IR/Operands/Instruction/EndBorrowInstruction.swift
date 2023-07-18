import Core

/// Ends the lifetime of a borrow.
public struct EndBorrowInstruction: Instruction {

  /// The borrow whose lifetime is ended.
  public private(set) var borrow: Operand

  /// The site of the code corresponding to that instruction.
  public let site: SourceRange

  /// Creates an instance with the given properties.
  fileprivate init(borrow: Operand, site: SourceRange) {
    self.borrow = borrow
    self.site = site
  }

  public var types: [IR.LoweredType] { [] }

  public var operands: [Operand] { [borrow] }

  public mutating func replaceOperand(at i: Int, with new: Operand) {
    precondition(i == 0)
    borrow = new
  }

}

extension Module {

  /// Creates an `end_borrow` anchored at `site` that ends a borrow previously created by `borrow`.
  ///
  /// - Parameters:
  ///   - borrow: The borrow to end. Must be the result of `borrow`.
  func makeEndBorrow(_ borrow: Operand, at site: SourceRange) -> EndBorrowInstruction {
    precondition(borrow.instruction.map({ self[$0] is BorrowInstruction }) ?? false)
    return .init(borrow: borrow, site: site)
  }

}
