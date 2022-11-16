import Foundation

protocol CodableMetatypeWrapperProtocol: AnyObject {
  static var wrappedType: Codable.Type { get }
}

private class CodableMetatypeWrapper<T: Codable>: CodableMetatypeWrapperProtocol {
  static var wrappedType: Codable.Type { T.self }
}
extension Decodable where Self: Encodable {
  static var typeKey: String { NSStringFromClass(CodableMetatypeWrapper<Self>.self) }
}

public struct NodeKind: Codable, Equatable, Hashable {
  let value: Node.Type

  init(_ value: Node.Type) {
    self.value = value
  }
  
  public func encode(to encoder: Encoder) throws {
    var c = encoder.singleValueContainer()
    try c.encode(value.typeKey)
  }
  
  public init(from decoder: Decoder) throws {
    let typeKey = try decoder.singleValueContainer().decode(String.self)
    
    guard let wrapperClass = NSClassFromString(typeKey) as? CodableMetatypeWrapperProtocol.Type else {
      throw DecodingError.typeMismatch(
        NodeKind.self,
        .init(
          codingPath: decoder.codingPath,
          debugDescription: "Result of NSClassFromString was nil or was not a CodableMetatypeWrapperProtocol.",
          underlyingError: nil))
    }
    
    guard let nodeType = wrapperClass.wrappedType as? Node.Type else {
      throw DecodingError.typeMismatch(
        NodeKind.self,
        .init(
          codingPath: decoder.codingPath,
          debugDescription: "\(wrapperClass.wrappedType) is not a Node type.",
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
