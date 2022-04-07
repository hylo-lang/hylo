/// A generic size parameter declaration.
public struct GenericSizeParamDecl: Decl, SourceRepresentable {

  public var range: SourceRange?

  /// The identifier of the parameter.
  public var identifier: Identifier

  public func accept<V: DeclVisitor>(_ visitor: inout V) -> V.Result {
    visitor.visit(genericSizeParam: self)
  }

}
