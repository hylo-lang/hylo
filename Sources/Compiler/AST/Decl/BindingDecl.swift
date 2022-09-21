/// A binding declaration.
public struct BindingDecl: Decl {

  public static let kind = NodeKind.bindingDecl

  /// The attributes of the declaration, if any.
  public var attributes: [SourceRepresentable<Attribute>]

  /// The access modifier of the declaration, if any.
  public var accessModifier: SourceRepresentable<AccessModifier>?

  /// The member modifier of the declaration.
  public var memberModifier: SourceRepresentable<MemberModifier>?

  /// The pattern of the declaration.
  public var pattern: NodeID<BindingPattern>

  /// The initializer of the declaration, if any.
  public var initializer: AnyExprID?

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

  /// Returns whether the declaration is public.
  public var isPublic: Bool { accessModifier?.value == .public }

  /// Returns whether the declaration denotes a static method.
  public var isStatic: Bool { memberModifier?.value == .static }

}
