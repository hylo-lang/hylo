/// The expression of an existential type.
public struct ExistentialTypeExpr: TypeExpr {

  public var range: SourceRange?

  /// The traits to which the witness conforms.
  public var traits: TraitComposition

  /// The where clause of the expression, if any.
  public var whereClause: WhereClause?

  public func accept<V: TypeExprVisitor>(_ visitor: inout V) -> V.Result {
    visitor.visit(existential: self)
  }

}
