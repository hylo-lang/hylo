/// A sequence of binary operations.
public enum SequenceExpr: Expr {

  public typealias UnfoldedTail = [(operator: SourceRepresentable<Identifier>, rhs: AnyExprID)]

  public static let kind = NodeKind.sequenceExpr

  /// A sequence that has not been folded to a tree yet.
  ///
  /// The associated value is the first operand and an array of subsequent operator/operand pairs.
  case unfolded(head: AnyExprID, tail: UnfoldedTail)

  /// The root of a folded sequence of binary operations.
  case root(AnyExprID)

}
