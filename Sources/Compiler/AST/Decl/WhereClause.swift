import Utils

/// A where clause.
public struct WhereClause: Hashable {

  /// The expression of a type constraint defined in a generic clause.
  public enum ConstraintExpr: Hashable {

    /// An equality constraint involving one or two skolems.
    case equality(
      l: AnyTypeExprID,
      r: AnyTypeExprID)

    /// A conformance constraint on a skolem.
    case conformance(
      l: NodeID<NameTypeExpr>,
      traits: TraitComposition)

    /// A size constraint on a value parameter.
    case size(AnyExprID)

  }

  /// The constraint expressions in the clause.
  public var constraints: [SourceRepresentable<ConstraintExpr>]

}
