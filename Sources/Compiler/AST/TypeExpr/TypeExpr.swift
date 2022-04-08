/// A type expression wrapped in a type-erased container.
public indirect enum TypeExpr: Hashable {

  case async(AsyncTypeExpr)

  case conformanceLens(ConformanceLensTypeExpr)

  case existential(ExistentialTypeExpr)

  case indirect(IndirectTypeExpr)

  case lambda(LambdaTypeExpr)

  case name(NameTypeExpr)

  case param(ParamTypeExpr)

  case storedProjection(StoredProjectionTypeExpr)

  case tuple(TupleTypeExpr)

  case union(UnionTypeExpr)

  /// The value wrapped in this container.
  public var base: Any {
    switch self {
    case let .async(e):             return e
    case let .conformanceLens(e):   return e
    case let .existential(e):       return e
    case let .indirect(e):          return e
    case let .lambda(e):            return e
    case let .name(e):              return e
    case let .param(e):             return e
    case let .storedProjection(e):  return e
    case let .tuple(e):             return e
    case let .union(e):             return e
    }
  }

  /// Accepts the specified visitor.
  public func accept<V: TypeExprVisitor>(_ visitor: inout V) -> V.Result {
    switch self {
    case let .async(e):             return visitor.visit(async: e)
    case let .conformanceLens(e):   return visitor.visit(conformanceLens: e)
    case let .existential(e):       return visitor.visit(existential: e)
    case let .indirect(e):          return visitor.visit(indirect: e)
    case let .lambda(e):            return visitor.visit(lambda: e)
    case let .name(e):              return visitor.visit(name: e)
    case let .param(e):             return visitor.visit(param: e)
    case let .storedProjection(e):  return visitor.visit(storedProjection: e)
    case let .tuple(e) :            return visitor.visit(tuple: e)
    case let .union(e):             return visitor.visit(union: e)
    }
  }

}
