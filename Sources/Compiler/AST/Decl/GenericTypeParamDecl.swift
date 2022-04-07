/// A generic type parameter declaration.
public struct GenericTypeParamDecl: Decl, SourceRepresentable {

  public var range: SourceRange?

  /// The identifier of the parameter.
  public var identifier: Identifier

  /// The conformances listed in the declaration.
  public var conformances: [NameTypeExpr]

  public func accept<V: DeclVisitor>(_ visitor: inout V) -> V.Result {
    visitor.visit(genericTypeParam: self)
  }

}
