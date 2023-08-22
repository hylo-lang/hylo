import Core

/// Ends the lifetime of an access.
public struct EndAccess: Instruction {

  /// The access whose lifetime is ended.
  public private(set) var start: Operand

  /// The site of the code corresponding to that instruction.
  public let site: SourceRange

  /// Creates an instance with the given properties.
  fileprivate init(start: Operand, site: SourceRange) {
    self.start = start
    self.site = site
  }

  public var operands: [Operand] {
    [start]
  }

  public mutating func replaceOperand(at i: Int, with new: Operand) {
    precondition(i == 0)
    start = new
  }

}

extension Module {

  /// Creates an `end_access` anchored at `site` that ends an access previously created by `start`.
  ///
  /// - Parameters:
  ///   - access: The borrow to end. Must be the result of `borrow`.
  func makeEndAccess(_ start: Operand, at site: SourceRange) -> EndAccess {
    precondition(start.instruction.map({ self[$0] is Access }) ?? false)
    return .init(start: start, site: site)
  }

}
