/// A placeholder representing a semantically erroneous expression in the AST.
public struct ErrorExpr: Expr {

  public static let kind = NodeKind.errorExpr

  public init() {}

}
