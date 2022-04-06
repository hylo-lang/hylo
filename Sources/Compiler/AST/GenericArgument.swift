/// The expression of an argument to a generic entity.
public enum GenericArgument: SourceRepresentable {

  case type(TypeExpr)

  case size(Expr)

  public var range: SourceRange? {
    switch self {
    case let .type(expr):
      return expr.range
    case let .size(expr):
      return expr.range
    }
  }

}
