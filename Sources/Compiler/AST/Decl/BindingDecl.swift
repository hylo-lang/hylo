/// A binding declaration.
public struct BindingDecl: Decl {

  public static let kind = NodeKind.bindingDecl

  /// The access modifier of the declaration, if any.
  public var accessModifier: SourceRepresentable<AccessModifier>?

  /// The member modifiers of the declaration.
  public var memberModifiers: [SourceRepresentable<MemberModifier>]

  /// The pattern of the declaration.
  public var pattern: NodeID<BindingPattern>

  /// The initializer of the declaration, if any.
  public var initializer: AnyExprID?

  public init(
    accessModifier: SourceRepresentable<AccessModifier>? = nil,
    memberModifiers: [SourceRepresentable<MemberModifier>] = [],
    pattern: NodeID<BindingPattern>,
    initializer: AnyExprID? = nil
  ) {
    self.accessModifier = accessModifier
    self.memberModifiers = memberModifiers
    self.pattern = pattern
    self.initializer = initializer
  }

  /// Returns whether the declaration denotes a static method.
  public var isStatic: Bool { memberModifiers.contains(where: { $0.value == .static }) }

}
