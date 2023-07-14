/// A type alias declaration.
public struct TypeAliasDecl: SingleEntityDecl, GenericDecl, TypeScope, GenericScope {

  public static let manglingOperator = ManglingOperator.typealiasDecl

  public let site: SourceRange

  /// The access modifier of the declaration, if any.
  public let accessModifier: SourceRepresentable<AccessModifier>

  /// The identifier of the alias.
  public let identifier: SourceRepresentable<Identifier>

  /// The generic clause of the declaration, if any.
  public let genericClause: SourceRepresentable<GenericClause>?

  /// The expression of the aliased type.
  public let aliasedType: AnyTypeExprID

  /// Creates an instance with the given properties.
  public init(
    accessModifier: SourceRepresentable<AccessModifier>,
    identifier: SourceRepresentable<Identifier>,
    genericClause: SourceRepresentable<GenericClause>?,
    aliasedType: AnyTypeExprID,
    site: SourceRange
  ) {
    self.site = site
    self.accessModifier = accessModifier
    self.identifier = identifier
    self.genericClause = genericClause
    self.aliasedType = aliasedType
  }

  public var baseName: String { identifier.value }

}
