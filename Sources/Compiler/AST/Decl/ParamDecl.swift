/// A parameter declaration in a function or subscript declaration.
public struct ParamDecl: Decl, SourceRepresentable {

  public var range: SourceRange?

  /// The label of the parameter.
  public var label: Identifier

  /// The identifier of the parameter.
  public var identifier: Identifier

  /// The type annotation of the declaration, if any.
  public var annotation: TypeExpr?

  /// The default value of the declaration, if any.
  public var defaultValue: Expr?

  public func accept<V: DeclVisitor>(_ visitor: inout V) -> V.Result {
    visitor.visit(param: self)
  }

}
