/// A node and the site of its introducer in program sources.
public struct Introduced<T: NodeIDProtocol>: NodeIDProtocol {
  public let introducerSite: SourceRange
  public let value: T

  public var rawValue: NodeID.RawValue { value.rawValue }
  public var kind: NodeKind { value.kind }

  public init?<Other: NodeIDProtocol>(_ x: Other) {
    if let i = x as? Introduced {
      self = i
    } else {
      return nil
    }
  }

  public init(introducerSite: SourceRange, value: T) {
    self.introducerSite = introducerSite
    self.value = value
  }
}
