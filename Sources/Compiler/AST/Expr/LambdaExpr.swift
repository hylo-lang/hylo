/// A lambda.
public struct LambdaExpr: Hashable {

  /// The declaration of the underlying anonymous function.
  public var decl: DeclIndex<FunDecl>

}
