/// A (nominal) product type declaration.
public struct ProductTypeDecl: ExposableDecl, GenericDecl, SingleEntityDecl, GenericScope {

  public static let constructDescription = "product type declaration"

  public let site: SourceRange

  /// The access modifier of the declaration, if any.
  public let accessModifier: SourceRepresentable<AccessModifier>

  /// The identifier of the type.
  public let identifier: SourceRepresentable<Identifier>

  /// The generic clause of the declaration, if any.
  public let genericClause: SourceRepresentable<GenericClause>?

  /// The names of traits to which the type conforms.
  public let conformances: [NameExpr.ID]

  /// The member declarations in the lexical scope of the trait.
  public let members: [AnyDeclID]

  /// Creates an instance with the given properties.
  public init(
    accessModifier: SourceRepresentable<AccessModifier>,
    identifier: SourceRepresentable<Identifier>,
    genericClause: SourceRepresentable<GenericClause>?,
    conformances: [NameExpr.ID],
    members: [AnyDeclID],
    site: SourceRange
  ) {
    self.site = site
    self.accessModifier = accessModifier
    self.identifier = identifier
    self.genericClause = genericClause
    self.conformances = conformances
    self.members = members
  }

  public var baseName: String { identifier.value }

  public func validateForm(in ast: AST, into diagnostics: inout DiagnosticSet) {
    for m in members {
      ast.validateTypeMember(m, into: &diagnostics)
    }
  }

}
