import Utils

public struct NodeKind: Codable, Equatable, Hashable {
  let value: Node.Type

  public init(_ value: Node.Type) {
    self.value = value
  }
  
  public func encode(to encoder: Encoder) throws {
    try Utils.encode(value, to: encoder)
  }
  
  public init(from decoder: Decoder) throws {
    let t = try decodeMetatype(from: decoder)
    guard let nodeType = t as? Node.Type else {
      throw DecodingError.typeMismatch(
        NodeKind.self,
        .init(
          codingPath: decoder.codingPath,
          debugDescription: "\(t) is not a Node type.",
          underlyingError: nil))
      
    }
    self.value = nodeType
  }

  public static func == (l: Self, r: Self) -> Bool {
    return l.value == r.value
  }

  public func hash(into h: inout Hasher) {
    ObjectIdentifier(value).hash(into: &h)
  }
  
  static func == (l: Self, r: Node.Type) -> Bool {
    return l.value == r
  }
  
  static func != (l: Self, r: Node.Type) -> Bool {
    return l.value == r
  }

  static func == (l: Node.Type, r: Self) -> Bool {
    return l == r.value
  }
  
  static func != (l: Node.Type, r: Self) -> Bool {
    return l != r.value
  }

  static func ~=<N: Node>(pattern: N.Type, value: Self) -> Bool {
    value == NodeKind(N.self)
  }

}

extension Optional where Wrapped == NodeKind {
  static func == (l: Self, r: Node.Type) -> Bool {
    return l?.value == r
  }

  static func != (l: Self, r: Node.Type) -> Bool {
    return l?.value == r
  }

  static func == (l: Node.Type, r: Self) -> Bool {
    return l == r?.value
  }

  static func != (l: Node.Type, r: Self) -> Bool {
    return l != r?.value
  }

}
extension NodeKind: CustomStringConvertible {

  public var description: String { String(describing: value) }

}
