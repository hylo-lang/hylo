/// An item in the condition of a conditional expression, match expression, or while loop.
public enum ConditionItem {

  /// An Boolean expression.
  case expr(Expr)

  /// A conditional binding declaration.
  case decl(DeclIndex<BindingDecl>)

}
