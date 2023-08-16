/// A binding declaration.
public struct BindingDecl: ExposableDecl {

  public let site: SourceRange

  /// The attributes of the declaration.
  public let attributes: [SourceRepresentable<Attribute>]

  /// The access modifier of the declaration, if any.
  public let accessModifier: SourceRepresentable<AccessModifier>

  /// The member modifier of the declaration.
  public let memberModifier: SourceRepresentable<MemberModifier>?

  /// The pattern of the declaration.
  public let pattern: BindingPattern.ID

  /// The initializer of the declaration, if any.
  public let initializer: AnyExprID?

  /// Creates an instance with the given properties.
  public init(
    attributes: [SourceRepresentable<Attribute>] = [],
    accessModifier: SourceRepresentable<AccessModifier>,
    memberModifier: SourceRepresentable<MemberModifier>? = nil,
    pattern: BindingPattern.ID,
    initializer: AnyExprID?,
    site: SourceRange
  ) {
    self.site = site
    self.attributes = attributes
    self.accessModifier = accessModifier
    self.memberModifier = memberModifier
    self.pattern = pattern
    self.initializer = initializer
  }

  /// Returns whether the declaration denotes a static member.
  public var isStatic: Bool { memberModifier?.value == .static }

}
