/// A pattern wrapped in a type-erased container.
public indirect enum Pattern: Hashable {

  case binding(BindingPattern)

  case expr(ExprPattern)

  case name(NamePattern)

  case tuple(TuplePattern)

  case wildcard(WildcardPattern)

  /// The value wrapped in this container.
  public var base: Any {
    switch self {
    case let .binding(p):   return p
    case let .expr(p):      return p
    case let .name(p):      return p
    case let .tuple(p):     return p
    case let .wildcard(p):  return p
    }
  }

  /// Accepts the specified visitor.
  public func accept<V: PatternVisitor>(_ visitor: inout V) -> V.Result {
    switch self {
    case let .binding(p):   return visitor.visit(binding: p)
    case let .expr(p):      return visitor.visit(expr: p)
    case let .name(p):      return visitor.visit(name: p)
    case let .tuple(p):     return visitor.visit(tuple: p)
    case let .wildcard(p):  return visitor.visit(wildcard: p)
    }
  }

}
