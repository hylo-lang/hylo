/// The expression of an argument to a generic entity.
public enum GenericArgument: Hashable {

  case size(AnyExprID)

  case type(AnyTypeExprID)

}
