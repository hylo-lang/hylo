/// A function declaration.
public struct FunctionDecl: GenericDecl, GenericScope {

  public static let kind = NodeKind.functionDecl

  public enum Body: Codable {

    /// An expression body.
    case expr(AnyExprID)

    /// A block body.
    case block(NodeID<BraceStmt>)

  }

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

  /// The return type annotation of the function, if any.
  public let output: AnyTypeExprID?

  /// The body of the declaration, if any.
  public let body: Body?

  /// Indicates whether the declaration appears in an expression context.
  public let isInExprContext: Bool

  /// The declaration of the implicit parameters of the function, if any.
  ///
  /// This property is set during type checking. In a local function, it maps the names of the
  /// implicit and explicit captures to their respective declaration. In a non-static member
  /// function declaration, it contains a reference to the declaration of the implicit receiver
  /// parameter (i.e., `self`).
  public private(set) var implicitParameterDecls: [ImplicitParameter]

  /// The declaration of the implicit receiver parameter, if any.
  public var implicitReceiverDecl: NodeID<ParameterDecl>? {
    if let parameter = implicitParameterDecls.first, parameter.name == "self" {
      return NodeID(parameter.decl)
    } else {
      return nil
    }
  }

  /// Creates an instance with the given properties.
  public init(
    introducerRange: SourceRange?,
    attributes: [SourceRepresentable<Attribute>] = [],
    accessModifier: SourceRepresentable<AccessModifier>? = nil,
    memberModifier: SourceRepresentable<MemberModifier>? = nil,
    receiverEffect: SourceRepresentable<ReceiverEffect>? = nil,
    notation: SourceRepresentable<OperatorNotation>? = nil,
    identifier: SourceRepresentable<Identifier>? = nil,
    genericClause: SourceRepresentable<GenericClause>? = nil,
    explicitCaptures: [NodeID<BindingDecl>] = [],
    parameters: [NodeID<ParameterDecl>] = [],
    receiver: NodeID<ParameterDecl>? = nil,
    output: AnyTypeExprID? = nil,
    body: Body? = nil,
    isInExprContext: Bool = false
  ) {
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
    self.output = output
    self.body = body
    self.isInExprContext = isInExprContext

    if let r = receiver {
      implicitParameterDecls = [ImplicitParameter(name: "self", decl: AnyDeclID(r))]
    } else {
      implicitParameterDecls = []
    }
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
  public var isFFI: Bool {
    attributes.contains(where: { $0.value.name.value == "@_lowered_name" })
  }

  /// Incorporates the given implicit parameter declarations into `self`.
  ///
  /// - Requires: `self.implicitParameterDecls` is empty.
  internal mutating func incorporate(implicitParameterDecls: [ImplicitParameter]) {
    precondition(self.implicitParameterDecls.isEmpty)
    self.implicitParameterDecls = implicitParameterDecls
  }

  public func isWellFormed(in ast: AST) -> SuccessOrDiagnostics {
    var ds: [Diagnostic] = []

    if !isInExprContext {
      // Parameter declarations must have a type annotation.
      for p in parameters {
        if ast[p].annotation == nil {
          ds.append(.diagnose(missingTypeAnnotation: ast[p], in: ast))
        }
      }
    }

    return ds.isEmpty ? .success : .failure(ds)
  }

}
