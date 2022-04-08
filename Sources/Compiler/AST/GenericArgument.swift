/// The expression of an argument to a generic entity.
public enum GenericArgument: Hashable {

  case type(TypeExpr)

  case size(Expr)

}
