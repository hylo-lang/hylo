/// A function declaration.
public struct FunctionDecl: GenericDecl, GenericScope {

  public enum Body: Codable {

    /// An expression body.
    case expr(AnyExprID)

    /// A block body.
    case block(NodeID<BraceStmt>)

  }

  public let origin: SourceRange?

  /// The source range of the `fun` introducer, if any.
  public let introducerRange: SourceRange?

  /// The attributes of the declaration, if any.
  public let attributes: [SourceRepresentable<Attribute>]

  /// The access modifier of the declaration, if any.
  public let accessModifier: SourceRepresentable<AccessModifier>?

  /// The member modifier of the declaration.
  public let memberModifier: SourceRepresentable<MemberModifier>?

  /// The receiver effect of the function.
  public let receiverEffect: SourceRepresentable<ReceiverEffect>?

  /// The operator notation of the function.
  public let notation: SourceRepresentable<OperatorNotation>?

  /// The identifier of the function, if any.
  public let identifier: SourceRepresentable<Identifier>?

  /// The generic clause of the declaration, if any.
  public let genericClause: SourceRepresentable<GenericClause>?

  /// The explicit capture declarations of the function.
  public let explicitCaptures: [NodeID<BindingDecl>]

  /// The parameters of the function.
  ///
  /// These declarations must have a type annotation unless `self.isInExprContext` is `true`.
  public let parameters: [NodeID<ParameterDecl>]

  /// The declaration of the implicit receiver parameter, if any.
  public let receiver: NodeID<ParameterDecl>?

  /// The return type annotation of the function, if any.
  public let output: AnyTypeExprID?

  /// The body of the declaration, if any.
  public let body: Body?

  /// Indicates whether the declaration appears in an expression context.
  public let isInExprContext: Bool

  /// Creates an instance with the given properties.
  public init(
    introducerRange: SourceRange?, attributes: [SourceRepresentable<Attribute>] = [],
    accessModifier: SourceRepresentable<AccessModifier>? = nil,
    memberModifier: SourceRepresentable<MemberModifier>? = nil,
    receiverEffect: SourceRepresentable<ReceiverEffect>? = nil,
    notation: SourceRepresentable<OperatorNotation>? = nil,
    identifier: SourceRepresentable<Identifier>? = nil,
    genericClause: SourceRepresentable<GenericClause>? = nil,
    explicitCaptures: [NodeID<BindingDecl>] = [], parameters: [NodeID<ParameterDecl>] = [],
    receiver: NodeID<ParameterDecl>? = nil, output: AnyTypeExprID? = nil, body: Body? = nil,
    isInExprContext: Bool = false, origin: SourceRange?
  ) {
    self.origin = origin
    self.introducerRange = introducerRange
    self.attributes = attributes
    self.accessModifier = accessModifier
    self.memberModifier = memberModifier
    self.receiverEffect = receiverEffect
    self.notation = notation
    self.identifier = identifier
    self.genericClause = genericClause
    self.explicitCaptures = explicitCaptures
    self.parameters = parameters
    self.receiver = receiver
    self.output = output
    self.body = body
    self.isInExprContext = isInExprContext
  }

  /// Returns whether the declaration is public.
  public var isPublic: Bool { accessModifier?.value == .public }

  /// Returns whether the declaration denotes a static member function.
  public var isStatic: Bool { memberModifier?.value == .static }

  /// Returns whether the declaration denotes an `inout` member function.
  public var isInout: Bool { receiverEffect?.value == .inout }

  /// Returns whether the declaration denotes a `sink` member function.
  public var isSink: Bool { receiverEffect?.value == .sink }

  /// Returns whether `self` is a foreign function interface.
  public var isFFI: Bool { attributes.contains(where: { $0.value.name.value == "@_lowered_name" }) }

  public func validateForm(in ast: AST) -> SuccessOrDiagnostics {
    var report: [Diagnostic] = []

    if !isInExprContext {
      // Parameter declarations must have a type annotation.
      for p in parameters {
        if ast[p].annotation == nil {
          report.append(.diagnose(missingTypeAnnotation: ast[p], in: ast))
        }
      }
    }

    return report.isEmpty ? .success : .failure(report)
  }

}
