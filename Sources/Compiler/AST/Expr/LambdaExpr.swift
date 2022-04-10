/// A lambda.
public struct LambdaExpr: Expr {

  /// The declaration of the underlying anonymous function.
  public var decl: NodeIndex<FunDecl>

}
