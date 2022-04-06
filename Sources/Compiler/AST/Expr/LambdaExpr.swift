/// A lambda.
public struct LambdaExpr: Expr {

  public var range: SourceRange?

  /// The declaration of the underlying anonymous function.
  public var decl: DeclIndex<FunDecl>

}
