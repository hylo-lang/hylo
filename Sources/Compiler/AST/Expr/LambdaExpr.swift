/// A lambda.
public struct LambdaExpr: Expr {

  /// The declaration of the underlying anonymous function.
  public let decl: NodeID<FunDecl>

  public init(decl: NodeID<FunDecl>) {
    self.decl = decl
  }

}
