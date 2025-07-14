import Utils

/// A function declaration.
public struct FunctionDecl: CapturingDecl, ExposableDecl, GenericDecl, Sendable {

  public static let constructDescription = "function declaration"

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
  /// All parameters must have a type annotation unless `isInExprContext` is `true`.
  public let parameters: [ParameterDecl.ID]

  /// The declaration of the implicit receiver parameter, if any.
  public let receiver: ParameterDecl.ID?

  /// The return type annotation of the function, if any.
  public let output: AnyExprID?

  /// The body of the declaration, if any.
  public let body: FunctionBody?

  /// Indicates whether the declaration appears in an expression context.
  public let isInExprContext: Bool

  /// Properties about the API described by the function.
  public let api: APIFlags

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
    output: AnyExprID? = nil,
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
    self.api = .init(attributes)
  }

  /// `true` iff `self` is a definition of the entity that it declares.
  public var isDefinition: Bool { body != nil }

  /// `true` iff `self` denotes a static member function.
  public var isStatic: Bool { memberModifier?.value == .static }

  /// `true` iff `self` denotes an `inout` member function.
  public var isInout: Bool { receiverEffect?.value == .inout }

  /// `true` iff `self` denotes a `sink` member function.
  public var isSink: Bool { receiverEffect?.value == .sink }

  /// `true` iff `self` denotes a deinitializer.
  public var isDeinit: Bool { (identifier?.value == "deinit") && isSink && parameters.isEmpty }

  /// `true` iff `self` is a foreign function interface.
  public var isForeignInterface: Bool {
    api.contains(.isForeignInterface)
  }

  /// `self` iff `self` declares a function whose implementation is defined externally.
  public var isExternal: Bool {
    api.contains(.isExternal)
  }

  /// The part of the declaration that may have implicit captures.
  public var sourcesOfImplicitCaptures: [AnyNodeID] { body.map({ (b) in [b.base] }) ?? [] }

  public func validateForm(in ast: AST, reportingDiagnosticsTo log: inout DiagnosticSet) {
    if !isInExprContext {
      // Parameter declarations must have a type annotation.
      for p in parameters {
        if ast[p].annotation == nil {
          log.insert(.error(missingTypeAnnotation: ast[p], in: ast))
        }
      }
    }
  }

}

extension FunctionDecl {

  /// A set of properties about the API described by a function declaration.
  public struct APIFlags: OptionSet, Codable, RawRepresentable, Sendable {

    public typealias RawValue = UInt8

    public let rawValue: UInt8

    public init(rawValue: UInt8) {
      self.rawValue = rawValue
    }

    /// Creates an instance describing the API of a declaration with given `attributes`.
    public init(_ attributes: [SourceRepresentable<Attribute>]) {
      var result = APIFlags()
      for a in attributes {
        switch a.value.name.value {
        case "@ffi":
          result.insert(.isForeignInterface)
        case "@external":
          result.insert(.isExternal)
        default:
          continue
        }
      }
      self = result
    }

    /// The declaration is a foreign function interface.
    public static let isForeignInterface = APIFlags(rawValue: 1)

    /// The declaration defined externally.
    public static let isExternal = APIFlags(rawValue: 2)

  }

}
