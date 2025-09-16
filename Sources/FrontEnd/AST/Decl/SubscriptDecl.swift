/// A subscript declaration.
public struct SubscriptDecl: BundleDecl, CapturingDecl, GenericDecl {

  public static let constructDescription = "subscript declaration"

  public typealias Variant = SubscriptImpl

  public enum Introducer: Codable, Sendable {

    /// The standard subscript introducer.
    case `subscript`

    /// The property introducer.
    case property

  }

  public static let isCallable = true

  public let site: SourceRange

  /// The introducer of the declaration.
  public let introducer: SourceRepresentable<Introducer>

  /// The attributes of the declaration.
  public let attributes: [SourceRepresentable<Attribute>]

  /// The access modifier of the declaration, if any.
  public let accessModifier: SourceRepresentable<AccessModifier>

  /// The member modifier of the declaration.
  public let memberModifier: SourceRepresentable<MemberModifier>?

  /// The identifier of the subscript, if any.
  public let identifier: SourceRepresentable<Identifier>?

  /// The generic clause of the declaration, if any.
  public let genericClause: SourceRepresentable<GenericClause>?

  /// The explicit capture declarations of the subscript.
  public let explicitCaptures: [BindingDecl.ID]

  /// The parameters of the subscript, empty for a property declaration.
  ///
  /// These declarations must have a type annotation.
  public let parameters: [ParameterDecl.ID]

  /// The output type annotation of the subscript.
  public let output: AnyExprID

  /// The implementations of the subscript.
  public let impls: [SubscriptImpl.ID]

  /// Creates an instance with the given properties.
  public init(
    introducer: SourceRepresentable<Introducer>,
    attributes: [SourceRepresentable<Attribute>],
    accessModifier: SourceRepresentable<AccessModifier>,
    memberModifier: SourceRepresentable<MemberModifier>?,
    identifier: SourceRepresentable<Identifier>?,
    genericClause: SourceRepresentable<GenericClause>?,
    explicitCaptures: [BindingDecl.ID],
    parameters: [ParameterDecl.ID],
    output: AnyExprID,
    impls: [SubscriptImpl.ID],
    site: SourceRange
  ) {
    self.site = site
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

  /// Returns whether the declaration introduces a property.
  public var isProperty: Bool { introducer.value == .property }

  /// Returns whether the declaration denotes a static subscript.
  public var isStatic: Bool { memberModifier?.value == .static }

  /// The part of the declaration that may have implicit captures.
  public var sourcesOfImplicitCaptures: [AnyNodeID] { impls.map(AnyNodeID.init) }

  public func validateForm(in ast: AST, reportingDiagnosticsTo log: inout DiagnosticSet) {
    // Parameter declarations must have a type annotation.
    for p in parameters {
      if ast[p].annotation == nil {
        log.insert(.error(missingTypeAnnotation: ast[p], in: ast))
      }
    }

  }

}
