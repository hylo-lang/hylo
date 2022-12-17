/// A sequence of binary operations.
public struct SequenceExpr: Expr {

  /// The operator and right operand in an unfolded sequence of infix expressions.
  public struct TailElement: Codable {

    /// The operator.
    public var `operator`: NodeID<NameExpr>

    /// The right operand.
    public var operand: AnyExprID

    public init(operator: NodeID<NameExpr>, operand: AnyExprID) {
      self.operator = `operator`
      self.operand = operand
    }

  }

  public let origin: SourceRange?

  /// The leftmost operand of the expression.
  public let head: AnyExprID

  /// The sequence of operators and operands at the right of `head`.
  public let tail: [TailElement]

  /// Creates an instance with the given properties.
  public init(head: AnyExprID, tail: [TailElement], origin: SourceRange?) {
    self.origin = origin
    self.head = head
    self.tail = tail
  }

  public func validateForm(in ast: AST) -> SuccessOrDiagnostics {
    var report: [Diagnostic] = []

    for element in tail {
      // Operator notation must be `nil` or `.infix`.
      if let notation = ast[element.operator].name.value.notation, notation != .infix {
        report.append(
          .diagnose(
            invalidOperatorNotation: notation,
            expected: .infix,
            at: ast[element.operator].name.origin))
      }
    }

    return report.isEmpty ? .success : .failure(report)
  }

}
