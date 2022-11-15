/// A conformance lens.
public struct ConformanceLensTypeExpr: TypeExpr {

  /// The expression of the subject type.
  public let subject: AnyTypeExprID

  /// The expression of the trait in which the lens focuses.
  public let lens: AnyTypeExprID

  public init(subject: AnyTypeExprID, lens: AnyTypeExprID) {
    self.subject = subject
    self.lens = lens
  }

}
