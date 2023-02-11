/// A (nominal) product type declaration.
public struct ProductTypeDecl: SingleEntityDecl, GenericDecl, TypeScope, GenericScope {

  public let site: SourceRange

  /// The access modifier of the declaration, if any.
  public let accessModifier: SourceRepresentable<AccessModifier>?

  /// The identifier of the type.
  public let identifier: SourceRepresentable<Identifier>

  /// The generic clause of the declaration, if any.
  public let genericClause: SourceRepresentable<GenericClause>?

  /// The names of traits to which the type conforms.
  public let conformances: [NodeID<NameExpr>]

  /// The member declarations in the lexical scope of the trait.
  public let members: [AnyDeclID]

  /// The memberwise initializer of the type.
  public let memberwiseInit: NodeID<InitializerDecl>

  /// Creates an instance with the given properties.
  public init(
    accessModifier: SourceRepresentable<AccessModifier>?,
    identifier: SourceRepresentable<Identifier>,
    genericClause: SourceRepresentable<GenericClause>?,
    conformances: [NodeID<NameExpr>],
    members: [AnyDeclID],
    memberwiseInit: NodeID<InitializerDecl>,
    site: SourceRange
  ) {
    precondition(members.contains(AnyDeclID(memberwiseInit)))

    self.site = site
    self.accessModifier = accessModifier
    self.identifier = identifier
    self.genericClause = genericClause
    self.conformances = conformances
    self.members = members
    self.memberwiseInit = memberwiseInit
  }

  public var baseName: String { identifier.value }

  /// Returns whether the declaration is public.
  public var isPublic: Bool { accessModifier?.value != nil }

  public func validateForm(in ast: AST, into diagnostics: inout DiagnosticSet) {
    for m in members {
      ast.validateTypeMember(m, into: &diagnostics)
    }
  }

}
