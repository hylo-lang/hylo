import Utils

/// The ID of a value expression.
public protocol ExprID: NodeIDProtocol {}

extension NodeID: ExprID where Subject: Expr {}

/// The type-erased ID of a value expression.
public struct AnyExprID: ExprID {

  /// The underlying type-erased ID.
  let base: AnyNodeID

  /// Creates a type-erased ID from a value expression ID.
  public init<T: ExprID>(_ other: T) { base = AnyNodeID(other) }

  public var rawValue: Int { base.rawValue }

  public var kind: NodeKind { base.kind }

}
