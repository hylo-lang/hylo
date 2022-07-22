/// A conformance lens.
public struct ConformanceLensTypeExpr: TypeExpr {

  public static let kind = NodeKind.conformanceLensTypeExpr

  /// The expression of the subject type.
  public var subject: AnyTypeExprID

  /// The expression of the trait in which the lens focuses.
  public var lens: AnyTypeExprID

  public init(subject: AnyTypeExprID, lens: AnyTypeExprID) {
    self.subject = subject
    self.lens = lens
  }

}
