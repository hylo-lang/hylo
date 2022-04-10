/// A generic size parameter declaration.
public struct GenericSizeParamDecl: SingleEntityDecl {

  /// The identifier of the parameter.
  public var identifier: SourceRepresentable<Identifier>

  public var name: String { identifier.value }

}
