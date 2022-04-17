/// A binding declaration.
public struct BindingDecl: Decl {

  public static let kind = NodeKind.bindingDecl

  /// The access modifier of the declaration, if any.
  public var accessModifier: SourceRepresentable<AccessModifier>?

  /// The member modifiers of the declaration.
  public var memberModifiers: [SourceRepresentable<MemberModifier>]

  /// The pattern of the declaration.
  public var pattern: AnyPatternID

  /// The initializer of the declaration, if any.
  public var initializer: AnyExprID?

}
