/// A list of binary operations which has not yet been folded into a tree.
///
/// The operands all have even indices. The sub-expressions with odd indices are all (potentially
/// unresolved) references to binary operators.
public struct UnfoldedExpr: Expr {

  public var range: SourceRange?

  public var subexpressions: [Expr]

}
