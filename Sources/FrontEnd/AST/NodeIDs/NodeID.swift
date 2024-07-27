/// A type denoting the ID of a node.
public protocol NodeIDProtocol: Hashable, Codable, CustomStringConvertible {

  /// The raw value of the ID.
  var rawValue: NodeRawIdentity { get }

  /// The identifier of type of the referred node.
  var kind: NodeKind { get }

  /// Creates an instance with the same raw value as `x`, failing iff `x.kind` is incompatible with
  /// the kind of nodes that `Self` denotes.
  init?<Other: NodeIDProtocol>(_ x: Other)

}

extension NodeIDProtocol {

  /// `true` iff `self` denotes a generic scope.
  public var isGenericScope: Bool {
    kind.value is GenericScope.Type
  }

  public var description: String { "\(kind)(\(rawValue))" }

}

public protocol ConcreteNodeID: NodeIDProtocol {

  associatedtype Subject: Node

}

/// The type of a node ID's raw value.
public struct NodeRawIdentity: Hashable, Codable {

  public let bits: UInt64

  public init(base: Int, offset: Int) {
    precondition(base < (1 << 16))
    self.bits = UInt64(base) + (UInt64(offset) << 16)
  }

  public var base: Int { Int(self.bits & ((1 << 16) - 1)) }

  public var offset: Int { Int(self.bits >> 16) }

}

extension NodeRawIdentity: Comparable {

  public static func < (l: Self, r: Self) -> Bool {
    l.bits < r.bits
  }

}

extension NodeRawIdentity: ExpressibleByIntegerLiteral {

  public init(integerLiteral value: Int) {
    self.bits = UInt64(value)
  }

}

extension NodeRawIdentity: CustomStringConvertible {

  public var description: String { bits.description }

}

/// The ID of a node in an AST.
public struct NodeID<Subject: Node>: ConcreteNodeID {

  public let rawValue: NodeRawIdentity

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
  init(rawValue: NodeRawIdentity) {
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

extension Sequence where Element: NodeIDProtocol {

  /// Returns a sequence containing, in order, the elements of `self` that are IDs of a `Subject`.
  public func filter<Subject: Node>(
    _: Subject.Type
  ) -> LazyMapSequence<LazyFilterSequence<LazyMapSequence<Self, Subject.ID?>>, Subject.ID> {
    self.lazy.compactMap(Subject.ID.init(_:))
  }

  /// Returns the unique element in `self` that is an ID of `Subject`, if any.
  public func unique<Subject: Node>(_ s: Subject.Type) -> Subject.ID? {
    var result: Subject.ID? = nil
    for n in self.filter(s) {
      guard result == nil else { return nil }
      result = n
    }
    return result
  }

}
