/// A folded sequence expression.
public indirect enum FoldedSequenceExpr {

  /// An operator with its precedence.
  public typealias Callee = (expr: NodeID<NameExpr>, precedence: PrecedenceGroup?)

  case node(callee: Callee, left: FoldedSequenceExpr, right: FoldedSequenceExpr)

  case leaf(AnyExprID)

  /// Appends the RHS of a binary operation to `self`.
  mutating func append(callee: Callee, right: AnyExprID) {
    switch self {
    case .node(let lhsCallee, let lhsLeft, var lhsRight):
      if let l = lhsCallee.precedence {
        if let r = callee.precedence {
          // Both operators are in groups.
          if (l < r) || (l == r && l.associativity == .left) {
            self = .node(callee: callee, left: self, right: .leaf(right))
            return
          }

          if (l > r) || (l == r && l.associativity == .right) {
            lhsRight.append(callee: callee, right: right)
            self = .node(callee: lhsCallee, left: lhsLeft, right: lhsRight)
            return
          }
        } else {
          // Right operator is not in a group. Assume lowest precedence and left associativity.
          self = .node(callee: callee, left: self, right: .leaf(right))
          return
        }
      } else if callee.precedence != nil {
        // Only right operator is in a group. Assume higher precedence.
        lhsRight.append(callee: callee, right: right)
        self = .node(callee: lhsCallee, left: lhsLeft, right: lhsRight)
      } else {
        // Neither operator is in a group. Assume left associativity.
        self = .node(callee: callee, left: self, right: .leaf(right))
      }

    case .leaf:
      self = .node(callee: callee, left: self, right: .leaf(right))
    }
  }

}
