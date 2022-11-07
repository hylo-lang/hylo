/// An initializer declaration.
public struct InitializerDecl: GenericDecl, GenericScope {

  public static let kind = NodeKind.initializerDecl

  /// The introducer of an initializer declaration.
  public enum Introducer: Codable {

    /// The initializer introducer, `init`.
    case `init`

    /// The memberwise initializer introducer, `memberwise init`
    case memberwiseInit

  }

  /// The introducer of the declaration.
  public let introducer: SourceRepresentable<Introducer>

  /// The attributes of the declaration, if any.
  public let attributes: [SourceRepresentable<Attribute>]

  /// The access modifier of the declaration, if any.
  public let accessModifier: SourceRepresentable<AccessModifier>?

  /// The generic clause of the declaration, if any.
  public let genericClause: SourceRepresentable<GenericClause>?

  /// The parameters of the initializer.
  ///
  /// These declarations must have a type annotation.
  public let parameters: [NodeID<ParameterDecl>]

  /// The declaration of the implicit receiver parameter.
  public let receiver: NodeID<ParameterDecl>

  /// The body of the declaration, if any.
  public let body: NodeID<BraceStmt>?

  /// Creates an instance with the given properties.
  public init(
    introducer: SourceRepresentable<Introducer>,
    attributes: [SourceRepresentable<Attribute>],
    accessModifier: SourceRepresentable<AccessModifier>?,
    genericClause: SourceRepresentable<GenericClause>?,
    parameters: [NodeID<ParameterDecl>],
    receiver: NodeID<ParameterDecl>,
    body: NodeID<BraceStmt>?
  ) {
    precondition((introducer.value == .`init`) || (body == nil))

    self.introducer = introducer
    self.attributes = attributes
    self.accessModifier = accessModifier
    self.genericClause = genericClause
    self.parameters = parameters
    self.receiver = receiver
    self.body = body
  }

  /// Returns whether the declaration is public.
  public var isPublic: Bool { accessModifier?.value == .public }

}
