/// An argument of a function or subscript call.
public struct Argument: SourceRepresentable {

  public var range: SourceRange?

  /// A flag indicating whether the argument is passed `inout`.
  public var isInout: Bool

  /// The label of the argument, if any.
  public var label: Identifier?

  /// The value of the argument.
  public var value: Expr

}
