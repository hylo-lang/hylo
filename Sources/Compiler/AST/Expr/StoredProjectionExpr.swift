/// A stored projection expression.
public struct StoredProjectionExpr: Expr {

  public struct Introducer: SourceRepresentable {

    public enum Kind {

      case `let`

      case `inout`

    }

    public var range: SourceRange?

    public var kind: Kind

  }

  public var range: SourceRange?

  public var introducer: Introducer

  /// The expression of the captured projection.
  public var operand: Expr

}
