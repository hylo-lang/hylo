/// A node index.
public protocol NodeIndexProtocol: Hashable {

  /// The raw value of the index.
  var rawValue: NodeIndex.RawValue { get }

}

/// The index of a node in an AST.
public struct NodeIndex<T: Node>: NodeIndexProtocol {

  /// The raw type of a node index.
  public typealias RawValue = Int

  public let rawValue: RawValue

  internal init(rawValue: RawValue) {
    self.rawValue = RawValue
  }

  public static func == <T: NodeIndexProtocol>(l: Self, r: T) -> Bool {
    l.rawValue == r.rawValue
  }

  public static func == <T: NodeIndexProtocol>(l: T, r: Self) -> Bool {
    l.rawValue == r.rawValue
  }

  public static func != <T: NodeIndexProtocol>(l: Self, r: T) -> Bool {
    l.rawValue != r.rawValue
  }

  public static func != <T: NodeIndexProtocol>(l: T, r: Self) -> Bool {
    l.rawValue != r.rawValue
  }

}
