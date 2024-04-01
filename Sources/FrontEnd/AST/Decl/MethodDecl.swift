/// A method declaration.
public struct MethodDecl: BundleDecl, GenericDecl {

  public static let constructDescription = "method declaration"

  public typealias Variant = MethodImpl

  public static let isCallable = true

  public let site: SourceRange

  /// The site of the `fun` introducer.
  public let introducerSite: SourceRange

  /// The attributes of the declaration.
  public let attributes: [SourceRepresentable<Attribute>]

  /// The access modifier of the declaration, if any.
  public let accessModifier: SourceRepresentable<AccessModifier>

  /// The operator notation of the method.
  public let notation: SourceRepresentable<OperatorNotation>?

  /// The identifier of the method.
  public let identifier: SourceRepresentable<Identifier>

  /// The generic clause of the declaration, if any.
  public let genericClause: SourceRepresentable<GenericClause>?

  /// The parameters of the method.
  ///
  /// These declarations must have a type annotation.
  public let parameters: [ParameterDecl.ID]

  /// The return type annotation of the method, if any.
  public let output: AnyExprID?

  /// The implementations of the method.
  public let impls: [MethodImpl.ID]

  /// Creates an instance with the given properties.
  public init(
    introducerSite: SourceRange,
    attributes: [SourceRepresentable<Attribute>],
    accessModifier: SourceRepresentable<AccessModifier>,
    notation: SourceRepresentable<OperatorNotation>?,
    identifier: SourceRepresentable<Identifier>,
    genericClause: SourceRepresentable<GenericClause>?,
    parameters: [ParameterDecl.ID],
    output: AnyExprID?,
    impls: [MethodImpl.ID],
    site: SourceRange
  ) {
    self.site = site
    self.introducerSite = introducerSite
    self.attributes = attributes
    self.accessModifier = accessModifier
    self.notation = notation
    self.identifier = identifier
    self.genericClause = genericClause
    self.parameters = parameters
    self.output = output
    self.impls = impls
  }

  public func validateForm(in ast: AST, reportingDiagnosticsTo log: inout DiagnosticSet) {
    // Parameter declarations must have a type annotation.
    for p in parameters {
      if ast[p].annotation == nil {
        log.insert(.error(missingTypeAnnotation: ast[p], in: ast))
      }
    }
  }

}
