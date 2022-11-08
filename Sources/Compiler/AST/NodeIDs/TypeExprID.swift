import Utils

/// The ID of a type expression.
public protocol TypeExprID: NodeIDProtocol {}

extension NodeID: TypeExprID where Subject: TypeExpr {}

/// The type-erased ID of a type expression.
public struct AnyTypeExprID: TypeExprID {

  /// The underlying type-erased ID.
  public let base: AnyNodeID

  /// Creates a type-erased ID from a type expression ID.
  public init<T: TypeExprID>(_ other: T) {
    base = AnyNodeID(other)
  }

  public var rawValue: Int { base.rawValue }

  public var kind: NodeKind { base.kind }

}
