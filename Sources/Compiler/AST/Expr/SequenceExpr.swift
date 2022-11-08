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

  /// A tree encoding sub-expression ordering of an unfolded `SequenceExpr`.
  internal indirect enum Tree {

    typealias Operator = (name: SourceRepresentable<Identifier>, precedence: PrecedenceGroup?)

    case node(operator: Operator, left: Tree, right: Tree)

    case leaf(AnyExprID)

    /// Appends the RHS of an operation to `self`.
    mutating func append(operator operator_: Operator, rhs: AnyExprID) {
      switch self {
      case .node(let lhsOperator, let lhsLeft, var lhsRight):
        if let l = lhsOperator.precedence {
          if let r = operator_.precedence {
            // Both operators are in groups.
            if (l < r) || (l == r && l.associativity == .left) {
              self = .node(operator: operator_, left: self, right: .leaf(rhs))
              return
            }

            if (l > r) || (l == r && l.associativity == .right) {
              lhsRight.append(operator: operator_, rhs: rhs)
              self = .node(operator: lhsOperator, left: lhsLeft, right: lhsRight)
              return
            }
          } else {
            // Right operator is not in a group. Assume lowest precedence and left associativity.
            self = .node(operator: operator_, left: self, right: .leaf(rhs))
            return
          }
        } else if operator_.precedence != nil {
          // Only right operator is in a group. Assume higher precedence.
          lhsRight.append(operator: operator_, rhs: rhs)
          self = .node(operator: lhsOperator, left: lhsLeft, right: lhsRight)
        } else {
          // Neither operator is in a group. Assume left associativity.
          self = .node(operator: operator_, left: self, right: .leaf(rhs))
        }

      case .leaf:
        self = .node(operator: operator_, left: self, right: .leaf(rhs))
      }
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
