/// A match expression.
public struct MatchExpr: Hashable {

  /// A case in a match expression.
  public struct Case: ScopeOutliner, Hashable {

    public enum Body: Hashable {

      /// An expression body.
      case expr(Expr)

      /// A block body.
      case block(BraceStmt)

    }

    var scopeID: ScopeID

    /// The pattern of the case.
    public var pattern: SourceRepresentable<Pattern>

    /// The condition of the case, if any.
    public var condition: SourceRepresentable<Expr>?

    /// The body of the case.
    public var body: SourceRepresentable<Body>

  }

  /// The subject of the match.
  public var subject: SourceRepresentable<Expr>

  /// The cases of the match.
  public var cases: [SourceRepresentable<Case>]

}
