/// An expression ran in a future.
public struct AsyncExpr: Expr {

  public static let kind = NodeKind.asyncExpr

  /// The declaration of the underlying anonymous function.
  public let decl: NodeID<FunctionDecl>

  public init(decl: NodeID<FunctionDecl>) {
    self.decl = decl
  }

}
