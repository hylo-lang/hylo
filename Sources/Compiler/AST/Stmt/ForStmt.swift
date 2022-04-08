/// A for loop.
public struct ForStmt: ScopeOutliner, Hashable {

  var scopeID: ScopeID

  /// The conditional binding of the loop.
  public var binding: DeclIndex<BindingDecl>

  /// The iteration domain of the loop.
  public var domain: SourceRepresentable<Expr>

  /// The filter of the loop, if any.
  public var filter: SourceRepresentable<Expr>?

  /// The body of the loop.
  public var body: SourceRepresentable<BraceStmt>

}
