/// A value expression.
public indirect enum Expr: Hashable {

  case async(AsyncExpr)

  case await(AwaitExpr)

  case boolLiteral(BoolLiteralExpr)

  case charLiteral(CharLiteralExpr)

  case cond(CondExpr)

  case floatLiteral(FloatLiteralExpr)

  case funCall(FunCallExpr)

  case intLiteral(IntLiteralExpr)

  case lambda(LambdaExpr)

  case mapLiteral(MapLiteralExpr)

  case match(MatchExpr)

  case name(NameExpr)

  case `nil`(NilExpr)

  case storedProjection(StoredProjectionExpr)

  case stringLiteral(StringLiteralExpr)

  case subscriptCall(SubscriptCallExpr)

  case tuple(TupleExpr)

  case unfolded(UnfoldedExpr)

  /// The value wrapped in this container.
  public var base: Any {
    switch self {
    case let .async(e):             return e
    case let .await(e):             return e
    case let .boolLiteral(e):       return e
    case let .charLiteral(e):       return e
    case let .cond(e):              return e
    case let .floatLiteral(e):      return e
    case let .funCall(e):           return e
    case let .intLiteral(e):        return e
    case let .lambda(e):            return e
    case let .mapLiteral(e):        return e
    case let .match(e):             return e
    case let .name(e):              return e
    case let .nil(e):               return e
    case let .storedProjection(e):  return e
    case let .stringLiteral(e):     return e
    case let .subscriptCall(e):     return e
    case let .tuple(e):             return e
    case let .unfolded(e):          return e
    }
  }

  /// Accepts the specified visitor.
  public func accept<V: ExprVisitor>(_ visitor: inout V) -> V.Result {
    switch self {
    case let .async(e):             return visitor.visit(async: e)
    case let .await(e):             return visitor.visit(await: e)
    case let .boolLiteral(e):       return visitor.visit(boolLiteral: e)
    case let .charLiteral(e):       return visitor.visit(charLiteral: e)
    case let .cond(e):              return visitor.visit(cond: e)
    case let .floatLiteral(e):      return visitor.visit(floatLiteral: e)
    case let .funCall(e):           return visitor.visit(funCall: e)
    case let .intLiteral(e):        return visitor.visit(intLiteral: e)
    case let .lambda(e):            return visitor.visit(lambda: e)
    case let .mapLiteral(e):        return visitor.visit(mapLiteral: e)
    case let .match(e):             return visitor.visit(match: e)
    case let .name(e):              return visitor.visit(name: e)
    case let .nil(e):               return visitor.visit(nil: e)
    case let .storedProjection(e):  return visitor.visit(storedProjection: e)
    case let .stringLiteral(e):     return visitor.visit(stringLiteral: e)
    case let .subscriptCall(e):     return visitor.visit(subscriptCall: e)
    case let .tuple(e):             return visitor.visit(tuple: e)
    case let .unfolded(e):          return visitor.visit(unfolded: e)
    }
  }

}
