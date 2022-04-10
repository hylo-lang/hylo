/// A generic type parameter declaration.
public struct GenericTypeParamDecl: Decl {

  /// The identifier of the parameter.
  public var identifier: SourceRepresentable<Identifier>

  /// The conformances listed in the declaration.
  public var conformances: [NodeIndex<NameTypeExpr>]

}
