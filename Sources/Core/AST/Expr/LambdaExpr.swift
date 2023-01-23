/// A lambda.
public struct LambdaExpr: Expr {

  public let site: SourceRange

  /// The declaration of the underlying anonymous function.
  public let decl: NodeID<FunctionDecl>

  public init(decl: NodeID<FunctionDecl>, site: SourceRange) {
    self.site = site
    self.decl = decl
  }

}
