/// The declaration of a subscript implementation.
/// ///
/// Instances of this type represent individual variant inside a subscript declaration.
public struct SubscriptImpl: Decl, LexicalScope {

  public static let isCallable = true

  public let site: SourceRange

  /// The introducer of the subscript.
  public let introducer: SourceRepresentable<AccessEffect>

  /// The declaration of the implicit receiver parameter, if any.
  public let receiver: ParameterDecl.ID?

  /// The body of the subscript, if any.
  public let body: FunctionBody?

  /// Creates an instance with the given properties.
  public init(
    introducer: SourceRepresentable<AccessEffect>,
    receiver: ParameterDecl.ID?,
    body: FunctionBody?,
    site: SourceRange
  ) {
    self.site = site
    self.introducer = introducer
    self.receiver = receiver
    self.body = body
  }

  /// `true` iff `self` is a definition of the entity that it declares.
  public var isDefinition: Bool { body != nil }

}
