/// A generic size parameter declaration.
public struct GenericSizeParamDecl: Decl, SourceRepresentable {

  public var range: SourceRange?

  /// The identifier of the parameter.
  public var identifier: Identifier

}
