/// A namespace declaration.
public struct NamespaceDecl: ExposableDecl, SingleEntityDecl, LexicalScope {

  public static let constructDescription = "namespace declaration"

  public let site: SourceRange

  /// The site of the declaration's introducer.
  public let introducerSite: SourceRange

  /// The access modifier of the declaration, if any.
  public let accessModifier: SourceRepresentable<AccessModifier>

  /// The identifier of the namespace.
  public let identifier: SourceRepresentable<Identifier>

  /// The member declarations in the lexical scope of the namespace.
  public let members: [AnyDeclID]

  /// Creates an instance with the given properties.
  public init(
    introducerSite: SourceRange,
    accessModifier: SourceRepresentable<AccessModifier>,
    identifier: SourceRepresentable<Identifier>,
    members: [AnyDeclID],
    site: SourceRange
  ) {
    self.site = site
    self.introducerSite = introducerSite
    self.accessModifier = accessModifier
    self.identifier = identifier
    self.members = members
  }

  public var baseName: String { identifier.value }

  public func validateForm(in ast: AST, into diagnostics: inout DiagnosticSet) {
    for m in members {
      ast.validateGlobalScopeMember(m, atTopLevel: false, reportingDiagnosticsTo: &diagnostics)
    }
  }

}
