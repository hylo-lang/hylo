/// A subscript declaration.
public struct SubscriptDecl: GenericDecl, GenericScope {

  public enum Introducer: Codable {

    /// The standard subscript introducer.
    case `subscript`

    /// The property introducer.
    case property

  }

  public let origin: SourceRange

  /// The introducer of the declaration.
  public let introducer: SourceRepresentable<Introducer>

  /// The attributes of the declaration, if any.
  public let attributes: [SourceRepresentable<Attribute>]

  /// The access modifier of the declaration, if any.
  public let accessModifier: SourceRepresentable<AccessModifier>?

  /// The member modifier of the declaration.
  public let memberModifier: SourceRepresentable<MemberModifier>?

  /// The identifier of the subscript, if any.
  public let identifier: SourceRepresentable<Identifier>?

  /// The generic clause of the declaration, if any.
  public let genericClause: SourceRepresentable<GenericClause>?

  /// The explicit capture declarations of the subscript.
  public let explicitCaptures: [NodeID<BindingDecl>]

  /// The parameters of the subscript, unless the declaration denotes a computed property.
  ///
  /// These declarations must have a type annotation.
  public let parameters: [NodeID<ParameterDecl>]?

  /// The output type annotation of the subscript.
  public let output: AnyTypeExprID

  /// The implementations of the subscript.
  public let impls: [NodeID<SubscriptImplDecl>]

  /// Creates an instance with the given properties.
  public init(
    introducer: SourceRepresentable<Introducer>,
    attributes: [SourceRepresentable<Attribute>],
    accessModifier: SourceRepresentable<AccessModifier>?,
    memberModifier: SourceRepresentable<MemberModifier>?,
    identifier: SourceRepresentable<Identifier>?,
    genericClause: SourceRepresentable<GenericClause>?,
    explicitCaptures: [NodeID<BindingDecl>],
    parameters: [NodeID<ParameterDecl>]?,
    output: AnyTypeExprID,
    impls: [NodeID<SubscriptImplDecl>],
    origin: SourceRange
  ) {
    self.origin = origin
    self.introducer = introducer
    self.attributes = attributes
    self.accessModifier = accessModifier
    self.memberModifier = memberModifier
    self.identifier = identifier
    self.genericClause = genericClause
    self.explicitCaptures = explicitCaptures
    self.parameters = parameters
    self.output = output
    self.impls = impls
  }

  /// Returns whether the declaration is public.
  public var isPublic: Bool { accessModifier?.value == .public }

  /// Returns whether the declaration denotes a static subscript.
  public var isStatic: Bool { memberModifier?.value == .static }

  public func validateForm(in ast: AST, into diagnostics: inout Diagnostics) {
    // Parameter declarations must have a type annotation.
    for p in parameters ?? [] {
      if ast[p].annotation == nil {
        diagnostics.report(.error(missingTypeAnnotation: ast[p], in: ast))
      }
    }

  }

}
