/// A name referring to a resolved declaration.
public struct DeclReferenceExpr: Expr {

  public static let kind = NodeKind.declReferenceExpr

  /// The referred declaration
  public var decl: AnyDeclID

  public init(decl: AnyDeclID) {
    self.decl = decl
  }

}
