import Core

/// Ends the lifetime of an access.
public struct EndAccess: Instruction {

  /// The borrow whose lifetime is ended.
  public private(set) var borrow: Operand

  /// The site of the code corresponding to that instruction.
  public let site: SourceRange

  /// Creates an instance with the given properties.
  fileprivate init(borrow: Operand, site: SourceRange) {
    self.borrow = borrow
    self.site = site
  }

  public var operands: [Operand] {
    [borrow]
  }

  public mutating func replaceOperand(at i: Int, with new: Operand) {
    precondition(i == 0)
    borrow = new
  }

}

extension Module {

  /// Creates an `end_access` anchored at `site` that ends an access previously created by `access`.
  ///
  /// - Parameters:
  ///   - access: The borrow to end. Must be the result of `borrow`.
  func makeEndAccess(_ borrow: Operand, at site: SourceRange) -> EndAccess {
    precondition(borrow.instruction.map({ self[$0] is Access }) ?? false)
    return .init(borrow: borrow, site: site)
  }

}
