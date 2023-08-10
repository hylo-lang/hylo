import Utils

/// A function declaration.
public struct FunctionDecl: GenericDecl, GenericScope {

  public static let isCallable = true

  public let site: SourceRange

  /// The site of the `fun` introducer.
  public let introducerSite: SourceRange

  /// The attributes of the declaration.
  public let attributes: [SourceRepresentable<Attribute>]

  /// The access modifier of the declaration, if any.
  public let accessModifier: SourceRepresentable<AccessModifier>

  /// The member modifier of the declaration.
  public let memberModifier: SourceRepresentable<MemberModifier>?

  /// The access effect of the function's receiver.
  public let receiverEffect: SourceRepresentable<AccessEffect>?

  /// The operator notation of the function.
  public let notation: SourceRepresentable<OperatorNotation>?

  /// The identifier of the function, if any.
  public let identifier: SourceRepresentable<Identifier>?

  /// The generic clause of the declaration, if any.
  public let genericClause: SourceRepresentable<GenericClause>?

  /// The explicit capture declarations of the function.
  public let explicitCaptures: [BindingDecl.ID]

  /// The parameters of the function.
  ///
  /// These declarations must have a type annotation unless `self.isInExprContext` is `true`.
  public let parameters: [ParameterDecl.ID]

  /// The declaration of the implicit receiver parameter, if any.
  public let receiver: ParameterDecl.ID?

  /// The return type annotation of the function, if any.
  public let output: AnyTypeExprID?

  /// The body of the declaration, if any.
  public let body: FunctionBody?

  /// Indicates whether the declaration appears in an expression context.
  public let isInExprContext: Bool

  /// Creates an instance with the given properties.
  public init(
    introducerSite: SourceRange,
    attributes: [SourceRepresentable<Attribute>] = [],
    accessModifier: SourceRepresentable<AccessModifier>,
    memberModifier: SourceRepresentable<MemberModifier>? = nil,
    receiverEffect: SourceRepresentable<AccessEffect>? = nil,
    notation: SourceRepresentable<OperatorNotation>? = nil,
    identifier: SourceRepresentable<Identifier>? = nil,
    genericClause: SourceRepresentable<GenericClause>? = nil,
    explicitCaptures: [BindingDecl.ID] = [],
    parameters: [ParameterDecl.ID] = [],
    receiver: ParameterDecl.ID? = nil,
    output: AnyTypeExprID? = nil,
    body: FunctionBody? = nil,
    isInExprContext: Bool = false,
    site: SourceRange
  ) {
    self.site = site
    self.introducerSite = introducerSite
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

  /// `true` iff `self` is a definition of the entity that it declares.
  public var isDefinition: Bool { body != nil }

  /// Returns whether the declaration denotes a static member function.
  public var isStatic: Bool { memberModifier?.value == .static }

  /// Returns whether the declaration denotes an `inout` member function.
  public var isInout: Bool { receiverEffect?.value == .inout }

  /// Returns whether the declaration denotes a `sink` member function.
  public var isSink: Bool { receiverEffect?.value == .sink }

  /// Returns whether `self` is a foreign function interface.
  public var isForeignInterface: Bool {
    foreignName != nil
  }

  /// The name of this foreign function if this instance is a foreign function interface.
  public var foreignName: String? {
    if let a = attributes.first(where: { $0.value.name.value == "@ffi" }) {
      guard case .string(let n) = a.value.arguments[0] else { unreachable() }
      return n.value
    } else {
      return nil
    }
  }

  /// The LLVM name of this function if this instance has the `@_llvm` attribute.
  public var llvmName: String? {
    if let a = attributes.first(where: { $0.value.name.value == "@_llvm" }) {
      guard case .string(let n) = a.value.arguments[0] else { unreachable() }
      return n.value
    } else {
      return nil
    }
  }

  public func validateForm(in ast: AST, into diagnostics: inout DiagnosticSet) {
    if !isInExprContext {
      // Parameter declarations must have a type annotation.
      for p in parameters {
        if ast[p].annotation == nil {
          diagnostics.insert(.error(missingTypeAnnotation: ast[p], in: ast))
        }
      }
    }
  }

}
