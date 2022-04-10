/// A generic type parameter declaration.
public struct GenericTypeParamDecl: SingleEntityDecl {

  /// The identifier of the parameter.
  public var identifier: SourceRepresentable<Identifier>

  /// The conformances listed in the declaration.
  public var conformances: [NodeIndex<NameTypeExpr>]

  public var name: String { identifier.value }

}
