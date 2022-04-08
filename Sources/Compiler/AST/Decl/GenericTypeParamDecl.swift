/// A generic type parameter declaration.
public struct GenericTypeParamDecl: Decl {

  /// The identifier of the parameter.
  public var identifier: SourceRepresentable<Identifier>

  /// The conformances listed in the declaration.
  public var conformances: [SourceRepresentable<NameTypeExpr>]

  public var range: SourceRange?

  public func accept<V: DeclVisitor>(_ visitor: inout V) -> V.Result {
    visitor.visit(genericTypeParam: self)
  }

}
