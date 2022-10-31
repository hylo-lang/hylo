import Utils

/// The ID of a node outlining a lexical scope.
public protocol ScopeID: NodeIDProtocol {}

extension NodeID: ScopeID where Subject: LexicalScope {}

/// The type-erased ID of a node outlining a lexical scope.
public struct AnyScopeID: ScopeID {

  /// The underlying type-erased ID.
  let base: AnyNodeID

  /// Creates a type-erased ID from the ID of a node outlining a lexical scope.
  public init<T: ScopeID>(_ other: T) {
    base = AnyNodeID(other)
  }

  /// Converts `n` to a scope ID; fails if `n` denotes a node that is not a lexical scope.
  public init?<Other: NodeIDProtocol>(converting other: Other) {
    if other.kind <= .lexicalScope {
      base = AnyNodeID(other)
    } else {
      return nil
    }
  }

  public var rawValue: Int { base.rawValue }

  public var kind: NodeKind { base.kind }

}
