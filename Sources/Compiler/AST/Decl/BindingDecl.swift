/// A binding declaration.
public struct BindingDecl: Decl, SourceRepresentable {

  public var range: SourceRange?

  /// The access modifier of the declaration, if any.
  public var accessModifier: AccessModifier?

  /// The member modifiers of the declaration.
  public var memberModifiers: [MemberModifier]

  /// The binding pattern of the declaration.
  public var pattern: BindingPattern

  /// The initializer of the declaration, if any.
  public var initializer: Expr?

}
