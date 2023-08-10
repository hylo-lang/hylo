/// A generic parameter declaration.
public struct GenericParameterDecl: SingleEntityDecl, ConstrainedGenericTypeDecl {

  public let site: SourceRange

  /// The identifier of the parameter.
  public let identifier: SourceRepresentable<Identifier>

  /// The conformances listed in the declaration.
  ///
  /// If this list contains exactly one name referring to a type, then `self` declares a value
  /// parameter of that type. Otherwise, it declares a generic type parameter and the names of
  /// this list refer to the traits to which it must conform.
  public let conformances: [NameExpr.ID]

  /// The default value of the declaration, if any.
  public let defaultValue: AnyExprID?

  public init(
    identifier: SourceRepresentable<Identifier>,
    conformances: [NameExpr.ID] = [],
    defaultValue: AnyExprID? = nil,
    site: SourceRange
  ) {
    self.site = site
    self.identifier = identifier
    self.conformances = conformances
    self.defaultValue = defaultValue
  }

  public var baseName: String { identifier.value }

}
