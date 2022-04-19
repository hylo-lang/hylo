import Utils

/// The ID of a value expression.
public protocol ExprID: NodeIDProtocol {}

extension NodeID: ExprID where T: Expr {}

/// The type-erased ID of a value expression.
public struct AnyExprID: ExprID {

  /// The underlying type-erased ID.
  var base: AnyNodeID

  /// Creates a type-erased ID from a value expression ID.
  public init<T: ExprID>(_ other: T) {
    base = AnyNodeID(other)
  }

  public var rawValue: Int { base.rawValue }

  public var kind: NodeKind { base.kind }

  public func accept<V: ExprVisitor>(_ visitor: inout V) -> V.Result {
    switch base.kind {
    case AsyncExpr.kind:
      return visitor.visit(async: NodeID(rawValue: base.rawValue))
    case AwaitExpr.kind:
      return visitor.visit(await: NodeID(rawValue: base.rawValue))
    case BoolLiteralExpr.kind:
      return visitor.visit(boolLiteral: NodeID(rawValue: base.rawValue))
    case CharLiteralExpr.kind:
      return visitor.visit(charLiteral: NodeID(rawValue: base.rawValue))
    case CondExpr.kind:
      return visitor.visit(cond: NodeID(rawValue: base.rawValue))
    case FloatLiteralExpr.kind:
      return visitor.visit(floatLiteral: NodeID(rawValue: base.rawValue))
    case FunCallExpr.kind:
      return visitor.visit(funCall: NodeID(rawValue: base.rawValue))
    case IntegerLiteralExpr.kind:
      return visitor.visit(integerLiteral: NodeID(rawValue: base.rawValue))
    case LambdaExpr.kind:
      return visitor.visit(lambda: NodeID(rawValue: base.rawValue))
    case MapLiteralExpr.kind:
      return visitor.visit(mapLiteral: NodeID(rawValue: base.rawValue))
    case MatchExpr.kind:
      return visitor.visit(match: NodeID(rawValue: base.rawValue))
    case MatchCaseExpr.kind:
      return visitor.visit(match: NodeID(rawValue: base.rawValue))
    case NameExpr.kind:
      return visitor.visit(name: NodeID(rawValue: base.rawValue))
    case NilExpr.kind:
      return visitor.visit(nil: NodeID(rawValue: base.rawValue))
    case StoredProjectionExpr.kind:
      return visitor.visit(storedProjection: NodeID(rawValue: base.rawValue))
    case StringLiteralExpr.kind:
      return visitor.visit(stringLiteral: NodeID(rawValue: base.rawValue))
    case SubscriptCallExpr.kind:
      return visitor.visit(subscriptCall: NodeID(rawValue: base.rawValue))
    case TupleExpr.kind:
      return visitor.visit(tuple: NodeID(rawValue: base.rawValue))
    case UnfoldedExpr.kind:
      return visitor.visit(unfolded: NodeID(rawValue: base.rawValue))
    default:
      unreachable()
    }
  }

}
