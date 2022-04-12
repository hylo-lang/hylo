/// The expression of an argument to a generic entity.
public enum GenericArgument: Hashable {

  case type(AnyTypeExprID)

  case size(AnyExprID)

}
