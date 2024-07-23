import Utils

/// The ID of a value expression.
public protocol ExprID: NodeIDProtocol {}

extension ExprID {

  /// `true` iff `self` denotes a literal expression.
  public var isLiteral: Bool {
    switch kind {
    case BooleanLiteralExpr.self:
      return true
    case BufferLiteralExpr.self:
      return true
    case FloatLiteralExpr.self:
      return true
    case IntegerLiteralExpr.self:
      return true
    case LambdaExpr.self:
      return true
    case MapLiteralExpr.self:
      return true
    case StringLiteralExpr.self:
      return true
    case UnicodeScalarLiteralExpr.self:
      return true
    default:
      return false
    }
  }

}

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

  public var rawValue: NodeRawIdentity { base.rawValue }

  public var kind: NodeKind { base.kind }

}
