/// A generic type parameter declaration.
public struct GenericTypeParamDecl: SingleEntityDecl {

  public static let kind = NodeKind.genericTypeParamDecl

  /// The identifier of the parameter.
  public var identifier: SourceRepresentable<Identifier>

  /// The conformances listed in the declaration.
  public var conformances: [NodeIndex<NameTypeExpr>]

  public var name: String { identifier.value }

}
