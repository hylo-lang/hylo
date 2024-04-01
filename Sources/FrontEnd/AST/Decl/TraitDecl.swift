/// A trait declaration.
///
/// - Note: `TraitDecl` does not conform to `GenericDecl`.
public struct TraitDecl: ExposableDecl, SingleEntityDecl, LexicalScope {

  public static let constructDescription = "trait declaration"

  public let site: SourceRange

  /// The site of the `trait` introducer.
  public let introducerSite: SourceRange

  /// The access modifier of the declaration, if any.
  public let accessModifier: SourceRepresentable<AccessModifier>

  /// The identifier of the trait.
  public let identifier: SourceRepresentable<Identifier>

  /// The names of traits which the trait refines.
  public let refinements: [NameExpr.ID]

  /// The member declarations in the lexical scope of the trait.
  public let members: [AnyDeclID]

  /// The declaration of the trait's `Self` parameter.
  public let receiver: GenericParameterDecl.ID

  /// Creates an instance with the given properties.
  public init(
    introducerSite: SourceRange,
    accessModifier: SourceRepresentable<AccessModifier>,
    identifier: SourceRepresentable<Identifier>,
    refinements: [NameExpr.ID],
    members: [AnyDeclID],
    selfParameterDecl: GenericParameterDecl.ID,
    site: SourceRange
  ) {
    precondition(members.contains(AnyDeclID(selfParameterDecl)))

    self.introducerSite = introducerSite
    self.site = site
    self.accessModifier = accessModifier
    self.identifier = identifier
    self.refinements = refinements
    self.receiver = selfParameterDecl
    self.members = members
  }

  public var baseName: String { identifier.value }

  public func validateForm(in ast: AST, reportingDiagnosticsTo log: inout DiagnosticSet) {
    for m in members {
      if let d = InitializerDecl.ID(m), ast[d].isMemberwise {
        log.insert(.error(unexpectedMemberwiseInitializerDecl: ast[d]))
      }
    }
  }

}

extension TraitDecl: GenericScope {

  public var genericParameters: [GenericParameterDecl.ID] {
    [receiver]
  }

}
