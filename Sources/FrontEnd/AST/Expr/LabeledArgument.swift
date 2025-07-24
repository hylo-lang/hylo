/// A static argument or an argument to a function or subscript call.
public struct LabeledArgument: Codable, Sendable {

  /// The label of the argument, if any.
  public let label: SourceRepresentable<Identifier>?

  /// The value of the argument.
  public let value: AnyExprID

  public init(label: SourceRepresentable<Identifier>?, value: AnyExprID) {
    self.label = label
    self.value = value
  }

}
