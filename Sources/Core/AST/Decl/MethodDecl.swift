/// A method declaration.
public struct MethodDecl: GenericDecl, GenericScope {

  public let origin: SourceRange

  /// The source range of the `fun` introducer.
  public let introducerRange: SourceRange

  /// The attributes of the declaration.
  public let attributes: [SourceRepresentable<Attribute>]

  /// The access modifier of the declaration, if any.
  public let accessModifier: SourceRepresentable<AccessModifier>?

  /// The operator notation of the method.
  public let notation: SourceRepresentable<OperatorNotation>?

  /// The identifier of the method.
  public let identifier: SourceRepresentable<Identifier>

  /// The generic clause of the declaration, if any.
  public let genericClause: SourceRepresentable<GenericClause>?

  /// The parameters of the method.
  ///
  /// These declarations must have a type annotation.
  public let parameters: [NodeID<ParameterDecl>]

  /// The return type annotation of the method, if any.
  public let output: AnyTypeExprID?

  /// The implementations of the method.
  public let impls: [NodeID<MethodImplDecl>]

  /// Creates an instance with the given properties.
  public init(
    introducerRange: SourceRange,
    attributes: [SourceRepresentable<Attribute>],
    accessModifier: SourceRepresentable<AccessModifier>?,
    notation: SourceRepresentable<OperatorNotation>?,
    identifier: SourceRepresentable<Identifier>,
    genericClause: SourceRepresentable<GenericClause>?,
    parameters: [NodeID<ParameterDecl>],
    output: AnyTypeExprID?,
    impls: [NodeID<MethodImplDecl>],
    origin: SourceRange
  ) {
    self.origin = origin
    self.introducerRange = introducerRange
    self.attributes = attributes
    self.accessModifier = accessModifier
    self.notation = notation
    self.identifier = identifier
    self.genericClause = genericClause
    self.parameters = parameters
    self.output = output
    self.impls = impls
  }

  /// Returns whether the declaration is public.
  public var isPublic: Bool { accessModifier?.value == .public }

  public func validateForm(in ast: AST, into diagnostics: inout Diagnostics) {
    // Parameter declarations must have a type annotation.
    for p in parameters {
      if ast[p].annotation == nil {
        diagnostics.report(.error(missingTypeAnnotation: ast[p], in: ast))
      }
    }
  }

}
