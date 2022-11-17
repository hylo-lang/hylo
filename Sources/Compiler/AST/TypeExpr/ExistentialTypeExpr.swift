/// The expression of an existential type.
public struct ExistentialTypeExpr: TypeExpr {

  /// The traits to which the witness conforms.
  public let traits: TraitComposition

  /// The where clause of the expression, if any.
  public let whereClause: SourceRepresentable<WhereClause>?

  public init(traits: TraitComposition, whereClause: SourceRepresentable<WhereClause>? = nil) {
    self.traits = traits
    self.whereClause = whereClause
  }

}
