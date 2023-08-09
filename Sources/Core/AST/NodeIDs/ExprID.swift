import Utils

/// The ID of a value expression.
public protocol ExprID: NodeIDProtocol {}

extension NodeID: ExprID where Subject: Expr {}

/// The type-erased ID of a value expression.
public struct AnyExprID: ExprID {

  /// The underlying type-erased ID.
  public let base: AnyNodeID

  /// Creates a type-erased ID from a value expression ID.
  public init<T: ExprID>(_ other: T) {
    base = AnyNodeID(other)
  }

  /// Creates an instance with the same raw value as `x` failing iff `!(x.kind is Expr)`.
  public init?<T: NodeIDProtocol>(_ x: T) {
    if x.kind.value is Expr.Type {
      self.base = AnyNodeID(x)
    } else {
      return nil
    }
  }

  public var rawValue: Int { base.rawValue }

  public var kind: NodeKind { base.kind }

}
