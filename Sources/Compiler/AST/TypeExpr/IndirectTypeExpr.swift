/// An indirect type expression.
public struct IndirectTypeExpr: TypeExpr {

  public var range: SourceRange?

  /// The operand.
  public var operand: TypeExpr

}
