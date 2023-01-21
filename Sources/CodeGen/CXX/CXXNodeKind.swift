import Utils

/// The type of a CXX node; a nominal wrapper for `CXXNode.Type` that adds conformances and
/// convenience APIs.
public struct CXXNodeKind: Equatable, Hashable {

  /// The underlying value.
  public let value: CXXNode.Type

  /// Creates an instance with the given underlying value.
  public init(_ value: CXXNode.Type) {
    self.value = value
  }

  /// Returns true iff `l` and `r` denote the same node type.
  public static func == (l: Self, r: Self) -> Bool {
    return l.value == r.value
  }

  /// Incorporates the value of `self` into `h`.
  public func hash(into h: inout Hasher) {
    ObjectIdentifier(value).hash(into: &h)
  }

  /// Returns true iff `l` and `r` denote the same node type.
  public static func == (l: Self, r: CXXNode.Type) -> Bool {
    return l.value == r
  }

  /// Returns true iff `l` and `r` do not denote the same node type.
  public static func != (l: Self, r: CXXNode.Type) -> Bool {
    return l.value == r
  }

  /// Returns true iff `l` and `r` denote the same node type.
  public static func == (l: CXXNode.Type, r: Self) -> Bool {
    return l == r.value
  }

  /// Returns true iff `l` and `r` do not denote the same node type.
  public static func != (l: CXXNode.Type, r: Self) -> Bool {
    return l != r.value
  }

  /// Returns true iff `me` and `pattern` denote the same node type.
  public static func ~= (pattern: CXXNode.Type, me: Self) -> Bool {
    me == pattern
  }

}

/// Extend heterogeneous equality comparison with CXXNode.Type to Optional<CXXNodeKind>.
extension Optional where Wrapped == CXXNodeKind {

  /// Returns true iff `l` and `r` denote the same node type.
  static func == (l: Self, r: CXXNode.Type) -> Bool {
    return l?.value == r
  }

  /// Returns true iff `l` and `r` do not denote the same node type.
  static func != (l: Self, r: CXXNode.Type) -> Bool {
    return l?.value == r
  }

  /// Returns true iff `l` and `r` denote the same node type.
  static func == (l: CXXNode.Type, r: Self) -> Bool {
    return l == r?.value
  }

  /// Returns true iff `l` and `r` do not denote the same node type.
  static func != (l: CXXNode.Type, r: Self) -> Bool {
    return l != r?.value
  }

}

extension CXXNodeKind: CustomStringConvertible {

  /// The name of the underlying Node type.
  public var description: String { String(describing: value) }

}
