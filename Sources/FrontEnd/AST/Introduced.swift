/// A node and the site of its introducer in program sources.
public struct Introduced<T: NodeIDProtocol>: Codable, Sendable {

  /// The site of `value`'s introducer.
  public let introducerSite: SourceRange

  /// A node introduced in program sources at `introducerSite`.
  public let value: T

  /// Creates an instance bundling `n` with the site from which its introducer was parsed.
  public init(_ n: T, at introducerSite: SourceRange) {
    self.introducerSite = introducerSite
    self.value = n
  }

}
