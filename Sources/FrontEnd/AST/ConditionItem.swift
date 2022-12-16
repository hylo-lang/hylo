/// An item in the condition of a conditional expression, match expression, or while loop.
public enum ConditionItem: Codable {

  /// An Boolean expression.
  case expr(AnyExprID)

  /// A conditional binding declaration.
  case decl(NodeID<BindingDecl>)

}
