/// A stored projection expression.
public struct StoredProjectionExpr: Expr {

  public static let kind = NodeKind.storedProjectionExpr

  public enum Introducer: Codable {

    case `let`

    case `inout`

  }

  public var introducer: SourceRepresentable<Introducer>

  /// The expression of the captured projection.
  public var operand: AnyExprID

  public init(
    introducer: SourceRepresentable<Introducer>,
    operand: AnyExprID
  ) {
    self.introducer = introducer
    self.operand = operand
  }

}
