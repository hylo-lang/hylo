/// An attribute.
public struct Attribute: Codable {

  /// An attribute argument.
  public struct Argument: Codable {

    /// The value of the argument.
    public let value: AnyExprID

    public init(value: AnyExprID) {
      self.value = value
    }

  }

  /// The name of the attribute.
  public let name: SourceRepresentable<String>

  /// The parameters of the attribute.
  public let arguments: [Argument]

  public init(name: SourceRepresentable<String>, arguments: [Argument] = []) {
    self.name = name
    self.arguments = arguments
  }

}
