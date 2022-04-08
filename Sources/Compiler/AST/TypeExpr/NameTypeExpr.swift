/// A name denoting a nominal type.
public struct NameTypeExpr: Hashable {

  /// The domain of the name, if it is qualified.
  public var domain: SourceRepresentable<TypeExpr>?

  /// The identifier of the referred type.
  public var identifier: SourceRepresentable<Identifier>

  /// The type and size arguments of the referred type.
  public var arguments: [SourceRepresentable<GenericArgument>]

}
