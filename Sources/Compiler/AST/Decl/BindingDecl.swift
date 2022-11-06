/// A binding declaration.
public struct BindingDecl: Decl {

  public static let kind = NodeKind.bindingDecl

  /// The attributes of the declaration, if any.
  public private(set) var attributes: [SourceRepresentable<Attribute>]

  /// The access modifier of the declaration, if any.
  public private(set) var accessModifier: SourceRepresentable<AccessModifier>?

  /// The member modifier of the declaration.
  public private(set) var memberModifier: SourceRepresentable<MemberModifier>?

  /// The pattern of the declaration.
  public let pattern: NodeID<BindingPattern>

  /// The initializer of the declaration, if any.
  public let initializer: AnyExprID?

  /// Creates an instance with the given properties.
  public init(
    attributes: [SourceRepresentable<Attribute>] = [],
    accessModifier: SourceRepresentable<AccessModifier>? = nil,
    memberModifier: SourceRepresentable<MemberModifier>? = nil,
    pattern: NodeID<BindingPattern>,
    initializer: AnyExprID? = nil
  ) {
    self.attributes = attributes
    self.accessModifier = accessModifier
    self.memberModifier = memberModifier
    self.pattern = pattern
    self.initializer = initializer
  }

  /// Incorporates the given decorations into `self`.
  ///
  /// - Precondition: `self` is undecorated.
  internal mutating func incorporate(
    attributes: [SourceRepresentable<Attribute>],
    accessModifier: SourceRepresentable<AccessModifier>?,
    memberModifier: SourceRepresentable<MemberModifier>?
  ) {
    precondition(self.accessModifier == nil)
    precondition(self.memberModifier == nil)
    precondition(self.attributes.isEmpty)
    self.attributes = attributes
    self.accessModifier = accessModifier
    self.memberModifier = memberModifier
  }

  /// Returns whether the declaration is public.
  public var isPublic: Bool { accessModifier?.value == .public }

  /// Returns whether the declaration denotes a static member.
  public var isStatic: Bool { memberModifier?.value == .static }

}
