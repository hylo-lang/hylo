/// The declaration of a method implementation.
///
/// Instances of this type represent individual variant inside a method declaration.
public struct MethodImpl: Decl, LexicalScope {

  public static let isCallable = true

  public let site: SourceRange

  /// The introducer of the method.
  public let introducer: SourceRepresentable<AccessEffect>

  /// The declaration of the implicit receiver parameter.
  public let receiver: ParameterDecl.ID

  /// The body of the method, if any.
  public let body: FunctionBody?

  /// Creates an instance with the given properties.
  public init(
    introducer: SourceRepresentable<AccessEffect>,
    receiver: ParameterDecl.ID,
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
