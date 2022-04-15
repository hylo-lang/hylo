/// A generic type parameter declaration.
public struct GenericTypeParamDecl: SingleEntityDecl {

  public static let kind = NodeKind.genericTypeParamDecl

  /// The identifier of the parameter.
  public var identifier: SourceRepresentable<Identifier>

  /// The conformances listed in the declaration.
  public var conformances: [NodeID<NameTypeExpr>]

  /// The default value of the declaration, if any.
  public var defaultValue: AnyTypeExprID?

  public var name: String { identifier.value }

}
