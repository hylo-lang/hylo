/// An expression ran in a future.
public struct SpawnExpr: Expr {

  public let site: SourceRange

  /// The declaration of the underlying anonymous function.
  public let decl: FunctionDecl.ID

  public init(decl: FunctionDecl.ID, site: SourceRange) {
    self.site = site
    self.decl = decl
  }

}
