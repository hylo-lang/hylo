import Utils

/// A where clause.
public struct WhereClause: Hashable {

  /// The expression of a type constraint defined in a generic clause.
  public enum ConstraintExpr: Hashable {

    /// An equality constraint involving one or two skolems.
    case equality(
      l: AnyTypeExprIndex,
      r: AnyTypeExprIndex)

    /// A conformance constraint on a skolem.
    case conformance(
      l: NodeIndex<NameTypeExpr>,
      traits: TraitComposition)

    /// A size constraint on a value parameter.
    case size(AnyExprIndex)

  }

  /// The constraint expressions in the clause.
  public var constraints: [SourceRepresentable<ConstraintExpr>]

}
