/// A lambda.
public struct LambdaExpr: Expr {

  public static let kind = NodeKind.lambdaExpr

  /// The declaration of the underlying anonymous function.
  public var decl: NodeID<FunDecl>

  public init(decl: NodeID<FunDecl>) {
    self.decl = decl
  }

}
