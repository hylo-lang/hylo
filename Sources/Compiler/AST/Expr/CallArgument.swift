/// An argument of a function or subscript call.
public struct CallArgument: Hashable {

  /// A flag indicating whether the argument is passed `inout`.
  public var isInout: Bool

  /// The label of the argument, if any.
  public var label: SourceRepresentable<Identifier>?

  /// The value of the argument.
  public var value: AnyExprID

  public init(
    isInout: Bool = false,
    label: SourceRepresentable<Identifier>? = nil,
    value: AnyExprID
  ) {
    self.isInout = isInout
    self.label = label
    self.value = value
  }

}
