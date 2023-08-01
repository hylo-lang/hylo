import Core

/// Ends the lifetime of a projection.
public struct EndProject: Instruction {

  /// The projection whose lifetime is ended.
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

  /// Creates an `end_project` anchored at `site` that ends the projection created by `start`.
  func makeEndProject(_ start: Operand, at anchor: SourceRange) -> EndProject {
    precondition(start.instruction.map({ self[$0] is Project }) ?? false)
    return .init(start: start, site: anchor)
  }

}
