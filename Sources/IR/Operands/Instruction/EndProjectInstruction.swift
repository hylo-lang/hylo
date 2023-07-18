import Core

/// Ends the lifetime of a projection.
public struct EndProjectInstruction: Instruction {

  /// The projection whose lifetime is ended.
  public private(set) var projection: Operand

  /// The site of the code corresponding to that instruction.
  public let site: SourceRange

  /// Creates an instance with the given properties.
  fileprivate init(projection: Operand, site: SourceRange) {
    self.projection = projection
    self.site = site
  }

  public var types: [IRType] { [] }

  public var operands: [Operand] { [projection] }

  public mutating func replaceOperand(at i: Int, with new: Operand) {
    precondition(i == 0)
    projection = new
  }

}

extension Module {

  /// Creates an `end_project` anchored at `site` that ends the projection created by `p`.
  func makeEndProject(
    _ p: Operand, anchoredAt anchor: SourceRange
  ) -> EndProjectInstruction {
    precondition(p.instruction.map({ self[$0] is ProjectInstruction }) ?? false)
    return .init(projection: p, site: anchor)
  }

}
