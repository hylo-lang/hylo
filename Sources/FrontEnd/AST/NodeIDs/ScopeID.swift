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

  /// Creates an instance referring to the same node as `x`, failing if `x` does not identify a
  /// lexical scope.
  public init?<Other: NodeIDProtocol>(_ x: Other) {
    if x.kind.value is LexicalScope.Type {
      base = AnyNodeID(x)
    } else {
      return nil
    }
  }

  public var rawValue: NodeRawIdentity { base.rawValue }

  public var kind: NodeKind { base.kind }

}
