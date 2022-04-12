/// A stored projection expression.
public struct StoredProjectionExpr: Expr {

  public static let kind = NodeKind.storedProjectionExpr

  public enum Introducer: Hashable {

    case `let`

    case `inout`

  }

  public var introducer: SourceRepresentable<Introducer>

  /// The expression of the captured projection.
  public var operand: AnyExprID

}
