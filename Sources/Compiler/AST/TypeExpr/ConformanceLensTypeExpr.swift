/// A conformance lens.
public struct ConformanceLens: TypeExpr {

  public var range: SourceRange?

  /// The expression of the lens' base.
  public var base: TypeExpr

  /// The name of the trait in which the lens focuses.
  public var trait: Identifier

}
