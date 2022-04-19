/// A list of binary operations which has not yet been folded into a tree.
///
/// The operands all have even indices. The sub-expressions with odd indices are all (potentially
/// unresolved) references to binary operators.
public struct UnfoldedExpr: Expr {

  public static let kind = NodeKind.unfoldedExpr

  /// The sub-expressions.
  ///
  /// - Requires: `subexpressions.count > 0 && subexpressions.count % 2 == 1`
  public var subexpressions: [AnyExprID]

  public init(subexpressions: [AnyExprID]) {
    precondition(subexpressions.count > 0 && subexpressions.count % 2 == 1)
    self.subexpressions = subexpressions
  }

}
