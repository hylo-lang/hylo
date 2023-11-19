/// A sequence of binary operations stored as a tree whose structure encodes the evaluation order.
///
/// Instances of this type are created during type checking for each `SequenceExpr` in the program
/// once the precedence groups of its operators have been determined. A tree is created by calling
/// `append(operator:right:)` to append an operator and its right operand to the sub-sequence
/// represented by `self`.
public indirect enum FoldedSequenceExpr: Equatable {

  /// The expression of an operator in the AST together with its precedence.
  public struct Operator: Equatable {

    /// The expression of an operator.
    public let expr: NameExpr.ID

    /// The precedence group of `expr`.
    public let precedence: PrecedenceGroup?

    /// Creates an instance with the given properties.
    public init(expr: NameExpr.ID, precedence: PrecedenceGroup?) {
      self.expr = expr
      self.precedence = precedence
    }

  }

  /// The application of an infix operator to its `left` and `right` operands.
  case infix(Operator, left: FoldedSequenceExpr, right: FoldedSequenceExpr)

  /// A leaf node representing some expression.
  case leaf(AnyExprID)

  /// The expression ID for `self`.
  public var exprID: AnyExprID {
    switch self {
    case .infix(let o, _, _):
      return AnyExprID(o.expr)!
    case .leaf(let id):
      return id
    }
  }

  /// Mutates `self` so that it represents the expression evaluated by appending `operator.expr`
  /// and `right` to `self`.
  ///
  /// This method uses `operator.precedence` to determine whether the whole expression represented
  /// by `self` should become the LHS of the new operation, or if the operator must apply to a
  /// sub-expression of `self`.
  ///
  /// Roughly, assuming `self` represents `a + b` and `+` has a lower precedence than `*`:
  /// - `self.append(+, c)` results in a tree representing `(a + b) + c`
  /// - `self.append(*, c)` results in a tree representing `a + (b * c)`
  public mutating func append(operator: Operator, right: AnyExprID) {
    switch self {
    case .infix(let lhsOperator, let lhsLeft, var lhsRight):
      // `self` represents a tree `(lhsLeft lhsOperator lhsRight)`: determine whether it should
      // become `self operator right` or `lhsLeft lhsOperator lhsRight.append(operator, right)`.
      if let l = lhsOperator.precedence {
        if let r = `operator`.precedence {
          // Both operators are in groups.
          if (l > r) || (l == r && l.associativity == .left) {
            self = .infix(`operator`, left: self, right: .leaf(right))
            return
          }

          if (l < r) || (l == r && l.associativity == .right) {
            lhsRight.append(operator: `operator`, right: right)
            self = .infix(lhsOperator, left: lhsLeft, right: lhsRight)
            return
          }
        } else {
          // Right operator is not in a group. Assume lowest precedence and left associativity.
          self = .infix(`operator`, left: self, right: .leaf(right))
          return
        }
      } else if `operator`.precedence != nil {
        // Only right operator is in a group. Assume higher precedence.
        lhsRight.append(operator: `operator`, right: right)
        self = .infix(lhsOperator, left: lhsLeft, right: lhsRight)
      } else {
        // Neither operator is in a group. Assume left associativity.
        self = .infix(`operator`, left: self, right: .leaf(right))
      }

    case .leaf:
      // `self` represents a leaf `left`: it should become `left operator right`.
      self = .infix(`operator`, left: self, right: .leaf(right))
    }
  }

}
