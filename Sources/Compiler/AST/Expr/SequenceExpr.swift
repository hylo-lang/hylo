/// A sequence of binary operations.
public enum SequenceExpr: Expr {

  public typealias UnfoldedTail = [TailElement]

  /// The operator and right operand in an unfolded sequence of infix expressions.
  public struct TailElement: Codable {

    /// The name of the operator.
    public var operatorName: SourceRepresentable<Identifier>

    /// The right operand.
    public var operand: AnyExprID

    public init(operatorName: SourceRepresentable<Identifier>, operand: AnyExprID) {
      self.operatorName = operatorName
      self.operand = operand
    }

  }

  public static let kind = NodeKind.sequenceExpr

  /// A sequence that has not been folded to a tree yet.
  ///
  /// The associated value is the first operand and an array of subsequent operator/operand pairs.
  case unfolded(head: AnyExprID, tail: [TailElement])

  /// The root of a folded sequence of binary operations.
  case root(AnyExprID)

}
