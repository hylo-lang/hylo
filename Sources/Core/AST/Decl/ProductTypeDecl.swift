/// A (nominal) product type declaration.
public struct ProductTypeDecl: SingleEntityDecl, GenericDecl, TypeScope, GenericScope {

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

  /// The memberwise initializer of the type.
  public let memberwiseInit: InitializerDecl.ID

  /// Creates an instance with the given properties.
  public init(
    accessModifier: SourceRepresentable<AccessModifier>?,
    identifier: SourceRepresentable<Identifier>,
    genericClause: SourceRepresentable<GenericClause>?,
    conformances: [NameExpr.ID],
    members: [AnyDeclID],
    memberwiseInit: InitializerDecl.ID,
    site: SourceRange
  ) {
    precondition(members.contains(AnyDeclID(memberwiseInit)))

    self.site = site
    // implicitly mark the product type as private
    self.accessModifier = accessModifier ?? SourceRepresentable(value: .private, range: site)
    self.identifier = identifier
    self.genericClause = genericClause
    self.conformances = conformances
    self.members = members
    self.memberwiseInit = memberwiseInit
  }

  public var baseName: String { identifier.value }

  /// Returns whether the declaration is public.
  public var isPublic: Bool { accessModifier.value == .public }

  public func validateForm(in ast: AST, into diagnostics: inout DiagnosticSet) {
    for m in members {
      ast.validateTypeMember(m, into: &diagnostics)
    }
  }

}
