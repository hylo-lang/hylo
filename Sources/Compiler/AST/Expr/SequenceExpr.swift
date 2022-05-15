/// A sequence of binary operations.
public enum SequenceExpr: Expr {

  public static let kind = NodeKind.sequenceExpr

  /// A sequence that has not been folded to a tree yet.
  ///
  /// The associated array has an odd number of elements greater than or equal to 3. Operands all
  /// all have even indices. Operators have odd indices and denote name expressions referring to
  /// binary operators.
  case unfolded([AnyExprID])

  /// The root of a folded sequence of binary operations.
  ///
  /// The associated value denotes a desugared binary operation.
  case root(AnyExprID)

}
