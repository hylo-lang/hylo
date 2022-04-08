/// A generic size parameter declaration.
public struct GenericSizeParamDecl: Decl {

  /// The identifier of the parameter.
  public var identifier: SourceRepresentable<Identifier>

  public var range: SourceRange?

  public func accept<V: DeclVisitor>(_ visitor: inout V) -> V.Result {
    visitor.visit(genericSizeParam: self)
  }

}
