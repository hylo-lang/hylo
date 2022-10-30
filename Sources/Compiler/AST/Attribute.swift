/// An attribute.
public struct Attribute: Codable {

  /// An attribute argument.
  public enum Argument: Codable {

    case string(SourceRepresentable<String>)

    case integer(SourceRepresentable<Int>)

  }

  /// The name of the attribute.
  private(set) var name: SourceRepresentable<String>

  /// The parameters of the attribute, if any.
  private(set) var arguments: [Argument]

  public init(name: SourceRepresentable<String>, arguments: [Argument] = []) {
    self.name = name
    self.arguments = arguments
  }

}
