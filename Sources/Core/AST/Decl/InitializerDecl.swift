/// An initializer declaration.
public struct InitializerDecl: ExposableDecl, GenericDecl, GenericScope {

  /// The introducer of an initializer declaration.
  public enum Introducer: Codable {

    /// The initializer introducer, `init`.
    case `init`

    /// The memberwise initializer introducer, `memberwise init`
    case memberwiseInit

  }

  public static let isCallable = true

  public let site: SourceRange

  /// The introducer of the declaration.
  public let introducer: SourceRepresentable<Introducer>

  /// The attributes of the declaration.
  public let attributes: [SourceRepresentable<Attribute>]

  /// The access modifier of the declaration, if any.
  public let accessModifier: SourceRepresentable<AccessModifier>

  /// The generic clause of the declaration, if any.
  public let genericClause: SourceRepresentable<GenericClause>?

  /// The parameters of the initializer.
  ///
  /// These declarations must have a type annotation.
  public let parameters: [ParameterDecl.ID]

  /// The declaration of the implicit receiver parameter.
  public let receiver: ParameterDecl.ID

  /// The body of the declaration, if any.
  public let body: BraceStmt.ID?

  /// Creates an instance with the given properties.
  public init(
    introducer: SourceRepresentable<Introducer>,
    attributes: [SourceRepresentable<Attribute>],
    accessModifier: SourceRepresentable<AccessModifier>,
    genericClause: SourceRepresentable<GenericClause>?,
    parameters: [ParameterDecl.ID],
    receiver: ParameterDecl.ID,
    body: BraceStmt.ID?,
    site: SourceRange
  ) {
    precondition((introducer.value == .`init`) || (body == nil))

    self.site = site
    self.introducer = introducer
    self.attributes = attributes
    self.accessModifier = accessModifier
    self.genericClause = genericClause
    self.parameters = parameters
    self.receiver = receiver
    self.body = body
  }

  /// `true` iff `self` is a definition of the entity that it declares.
  public var isDefinition: Bool { body != nil }

  /// Returns whether the declaration is a memberwise initializer.
  public var isMemberwise: Bool { introducer.value == .memberwiseInit }

  public func validateForm(in ast: AST, into diagnostics: inout DiagnosticSet) {
    // Parameter declarations must have a type annotation.
    for p in parameters {
      if ast[p].annotation == nil {
        diagnostics.insert(.error(missingTypeAnnotation: ast[p], in: ast))
      }
    }
  }

}
