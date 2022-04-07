/// A for loop.
public struct ForStmt: Stmt, ScopeOutliner {

  var scopeID: ScopeID

  public var range: SourceRange?

  /// The conditional binding of the loop.
  public var binding: DeclIndex<BindingDecl>

  /// The iteration domain of the loop.
  public var domain: Expr

  /// The filter of the loop, if any.
  public var filter: Expr?

  /// The body of the loop.
  public var body: BraceStmt

}

extension ForStmt: CustomStringConvertible {

  public var description: String {
    if let filter = filter {
      return "for \(binding) in \(domain) where \(filter) \(body)"
    } else {
      return "for \(binding) in \(domain) \(body)"
    }
  }

}
