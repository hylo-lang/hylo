/// A type denoting the ID of a node.
public protocol NodeIDProtocol: Hashable, Codable, CustomStringConvertible {

  /// The raw value of the ID.
  var rawValue: NodeID.RawValue { get }

  /// The identifier of type of the referred node.
  var kind: NodeKind { get }

}

extension NodeIDProtocol {

  public var description: String { "\(kind)(\(rawValue))" }

}

public protocol ConcreteNodeID: NodeIDProtocol {

  associatedtype Subject: Node

}

/// The ID of a node in an AST.
public struct NodeID<Subject: Node>: ConcreteNodeID {

  /// The type of a node ID's raw value.
  public typealias RawValue = Int

  public let rawValue: RawValue

  /// The dynamic type of node being referred to.
  public var kind: NodeKind { NodeKind(Subject.self) }

  /// Creates an instance with the same raw value as `x` failing iff `x.kind != Subject.kind`.
  public init?<Other: NodeIDProtocol>(_ x: Other) {
    if x.kind == Subject.kind {
      self.init(rawValue: x.rawValue)
    } else {
      return nil
    }
  }

  /// Creates an instance having the given raw value.
  ///
  /// The result can only be used correctly in an AST where the identified node has type `Subject`.
  public init(rawValue: RawValue) {
    self.rawValue = rawValue
  }

  public static func == <Other: NodeIDProtocol>(l: Self, r: Other) -> Bool {
    l.rawValue == r.rawValue
  }

  public static func == <Other: NodeIDProtocol>(l: Other, r: Self) -> Bool {
    l.rawValue == r.rawValue
  }

  public static func != <Other: NodeIDProtocol>(l: Self, r: Other) -> Bool {
    l.rawValue != r.rawValue
  }

  public static func != <Other: NodeIDProtocol>(l: Other, r: Self) -> Bool {
    l.rawValue != r.rawValue
  }

}
