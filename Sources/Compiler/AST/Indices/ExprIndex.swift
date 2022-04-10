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

  public var typeID: ObjectIdentifier { base.typeID }

  /// Returns a typed copy of this index, or `nil` if the type conversion failed.
  public func convert<T: Expr>(to: T.Type) -> NodeIndex<T>? {
    base.convert(to: T.self)
  }

  public func accept<V: ExprVisitor>(_ visitor: inout V) -> V.Result {
    switch base.typeID {
    case AsyncExpr.typeID:
      return visitor.visit(async: NodeIndex(rawValue: base.rawValue))
    case AwaitExpr.typeID:
      return visitor.visit(await: NodeIndex(rawValue: base.rawValue))
    case BoolLiteralExpr.typeID:
      return visitor.visit(boolLiteral: NodeIndex(rawValue: base.rawValue))
    case CharLiteralExpr.typeID:
      return visitor.visit(charLiteral: NodeIndex(rawValue: base.rawValue))
    case CondExpr.typeID:
      return visitor.visit(cond: NodeIndex(rawValue: base.rawValue))
    case FloatLiteralExpr.typeID:
      return visitor.visit(floatLiteral: NodeIndex(rawValue: base.rawValue))
    case FunCallExpr.typeID:
      return visitor.visit(funCall: NodeIndex(rawValue: base.rawValue))
    case IntLiteralExpr.typeID:
      return visitor.visit(intLiteral: NodeIndex(rawValue: base.rawValue))
    case LambdaExpr.typeID:
      return visitor.visit(lambda: NodeIndex(rawValue: base.rawValue))
    case MapLiteralExpr.typeID:
      return visitor.visit(mapLiteral: NodeIndex(rawValue: base.rawValue))
    case MatchExpr.typeID:
      return visitor.visit(match: NodeIndex(rawValue: base.rawValue))
    case NameExpr.typeID:
      return visitor.visit(name: NodeIndex(rawValue: base.rawValue))
    case NilExpr.typeID:
      return visitor.visit(nil: NodeIndex(rawValue: base.rawValue))
    case StoredProjectionExpr.typeID:
      return visitor.visit(storedProjection: NodeIndex(rawValue: base.rawValue))
    case StringLiteralExpr.typeID:
      return visitor.visit(stringLiteral: NodeIndex(rawValue: base.rawValue))
    case SubscriptCallExpr.typeID:
      return visitor.visit(subscriptCall: NodeIndex(rawValue: base.rawValue))
    case TupleExpr.typeID:
      return visitor.visit(tuple: NodeIndex(rawValue: base.rawValue))
    case UnfoldedExpr.typeID:
      return visitor.visit(unfolded: NodeIndex(rawValue: base.rawValue))
    default:
      unreachable()
    }
  }

}
