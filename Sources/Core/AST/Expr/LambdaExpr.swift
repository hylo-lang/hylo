/// A lambda.
public struct LambdaExpr: Expr {

  public let site: SourceRange

  /// The declaration of the underlying anonymous function.
  public let decl: FunctionDecl.ID

  public init(decl: FunctionDecl.ID, site: SourceRange) {
    self.site = site
    self.decl = decl
  }

}
