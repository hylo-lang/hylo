import Utils

/// An unambiguous textual description of a type, scope, or declaration.
public enum Symbol: Hashable {

  /// A declaration or lexical scope.
  case node(AnyNodeID)

  /// A canonical type.
  case type(AnyType)

  /// The node ID stored in the payload if `self` is `.node`. Otherwise, `nil`.
  public var node: AnyNodeID? {
    if case .node(let n) = self {
      return n
    } else {
      return nil
    }
  }

  /// The type stored in the payload if `self` is `.type`. Otherwise, `nil`.
  public var type: AnyType? {
    if case .type(let t) = self {
      return t
    } else {
      return nil
    }
  }

}
