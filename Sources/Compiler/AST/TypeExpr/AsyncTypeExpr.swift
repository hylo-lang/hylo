/// An asynchronous type expression.
public struct AsyncTypeExpr: TypeExpr {

  public var range: SourceRange?

  /// The operand.
  public var operand: TypeExpr

}
