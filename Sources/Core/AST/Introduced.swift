/// A node and the site of its introducer in program sources.
public struct Introduced<T: NodeIDProtocol>: Codable {
  public let introducerSite: SourceRange
  public let value: T

  public init(introducerSite: SourceRange, value: T) {
    self.introducerSite = introducerSite
    self.value = value
  }
}
