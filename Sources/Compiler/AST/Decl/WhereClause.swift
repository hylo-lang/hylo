import Utils

/// A where clause.
public struct WhereClause: Hashable {

  /// The expression of a type constraint defined in a generic clause.
  public enum ConstraintExpr: Hashable {

    /// An equality constraint involving one or two skolems.
    case equality(
      lhs: AnyTypeExprIndex,
      rhs: AnyTypeExprIndex)

    /// A conformance constraint on a skolem.
    case conformance(
      lhs: NodeIndex<NameTypeExpr>,
      rhs: SourceRepresentable<TraitComposition>)

    /// A size constraint on a value parameter.
    case size(AnyExprIndex)

  }

  /// The constraint expressions in the clause.
  public var constraints: [ConstraintExpr]

}
