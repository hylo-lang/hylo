/// A (nominal) product type declaration.
public struct ProductTypeDecl: ConformanceSource, GenericDecl, SingleEntityDecl {

  public static let constructDescription = "product type declaration"

  public let site: SourceRange

  /// The site of the `type` introducer.
  public let introducerSite: SourceRange

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
    introducerSite: SourceRange,
    accessModifier: SourceRepresentable<AccessModifier>,
    identifier: SourceRepresentable<Identifier>,
    genericClause: SourceRepresentable<GenericClause>?,
    conformances: [NameExpr.ID],
    members: [AnyDeclID],
    site: SourceRange
  ) {
    self.introducerSite = introducerSite
    self.site = site
    self.accessModifier = accessModifier
    self.identifier = identifier
    self.genericClause = genericClause
    self.conformances = conformances
    self.members = members
  }

  public var baseName: String { identifier.value }

  public func validateForm(in ast: AST, reportingDiagnosticsTo log: inout DiagnosticSet) {
    for m in members {
      ast.validateTypeMember(m, into: &log)
    }
  }

}
