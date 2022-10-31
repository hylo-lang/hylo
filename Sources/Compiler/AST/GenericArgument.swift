/// An argument of a function or subscript call.
public struct GenericArgument: Codable {

  /// The value of a generic argument.
  public enum Value: Codable {

    case expr(AnyExprID)

    case type(AnyTypeExprID)

  }

  /// The label of the argument, if any.
  public let label: SourceRepresentable<Identifier>?

  /// The value of the argument.
  public let value: Value

  public init(label: SourceRepresentable<Identifier>? = nil, value: Value) {
    self.label = label
    self.value = value
  }

}
