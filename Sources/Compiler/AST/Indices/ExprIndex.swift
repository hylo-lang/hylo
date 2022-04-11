import Utils

/// The index of a value expression.
public protocol ExprIndex: NodeIndexProtocol {}

extension NodeIndex: ExprIndex where T: Expr {}

/// The type-erased index of a value expression.
public struct AnyExprIndex: ExprIndex {

  /// The underlying type-erased index.
  var base: AnyNodeIndex

  /// Creates a type-erased index from a value expression index.
  public init<T: ExprIndex>(_ other: T) {
    base = AnyNodeIndex(other)
  }

  public var rawValue: Int { base.rawValue }

  public var kind: NodeKind { base.kind }

  /// Returns a typed copy of this index, or `nil` if the type conversion failed.
  public func convert<T: Expr>(to: T.Type) -> NodeIndex<T>? {
    base.convert(to: T.self)
  }

  public func accept<V: ExprVisitor>(_ visitor: inout V) -> V.Result {
    switch base.kind {
    case AsyncExpr.kind:
      return visitor.visit(async: NodeIndex(rawValue: base.rawValue))
    case AwaitExpr.kind:
      return visitor.visit(await: NodeIndex(rawValue: base.rawValue))
    case BoolLiteralExpr.kind:
      return visitor.visit(boolLiteral: NodeIndex(rawValue: base.rawValue))
    case CharLiteralExpr.kind:
      return visitor.visit(charLiteral: NodeIndex(rawValue: base.rawValue))
    case CondExpr.kind:
      return visitor.visit(cond: NodeIndex(rawValue: base.rawValue))
    case FloatLiteralExpr.kind:
      return visitor.visit(floatLiteral: NodeIndex(rawValue: base.rawValue))
    case FunCallExpr.kind:
      return visitor.visit(funCall: NodeIndex(rawValue: base.rawValue))
    case IntLiteralExpr.kind:
      return visitor.visit(intLiteral: NodeIndex(rawValue: base.rawValue))
    case LambdaExpr.kind:
      return visitor.visit(lambda: NodeIndex(rawValue: base.rawValue))
    case MapLiteralExpr.kind:
      return visitor.visit(mapLiteral: NodeIndex(rawValue: base.rawValue))
    case MatchExpr.kind:
      return visitor.visit(match: NodeIndex(rawValue: base.rawValue))
    case MatchCaseExpr.kind:
      return visitor.visit(match: NodeIndex(rawValue: base.rawValue))
    case NameExpr.kind:
      return visitor.visit(name: NodeIndex(rawValue: base.rawValue))
    case NilExpr.kind:
      return visitor.visit(nil: NodeIndex(rawValue: base.rawValue))
    case StoredProjectionExpr.kind:
      return visitor.visit(storedProjection: NodeIndex(rawValue: base.rawValue))
    case StringLiteralExpr.kind:
      return visitor.visit(stringLiteral: NodeIndex(rawValue: base.rawValue))
    case SubscriptCallExpr.kind:
      return visitor.visit(subscriptCall: NodeIndex(rawValue: base.rawValue))
    case TupleExpr.kind:
      return visitor.visit(tuple: NodeIndex(rawValue: base.rawValue))
    case UnfoldedExpr.kind:
      return visitor.visit(unfolded: NodeIndex(rawValue: base.rawValue))
    default:
      unreachable()
    }
  }

}
