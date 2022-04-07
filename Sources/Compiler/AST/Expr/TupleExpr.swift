/// A tuple expression.
public struct TupleExpr: Expr {

  /// An element in a tuple expression.
  public struct Element: SourceRepresentable {

    public var range: SourceRange?

    /// The label of the element.
    public var label: String?

    /// The value of the element.
    public var value: Expr

  }

  public var range: SourceRange?

  /// The elements of the tuple.
  public var elements: [Element]

  public func accept<V: ExprVisitor>(_ visitor: inout V) -> V.Result {
    visitor.visit(tupleExpr: self)
  }

}
