import Utils

/// A where clause.
public struct WhereClause: Hashable {

  /// The expression of a type constraint defined in a generic clause.
  public enum ConstraintExpr: Hashable {

    /// An equality constraint involving one or two skolems.
    case equality(
      lhs: SourceRepresentable<TypeExpr>,
      rhs: SourceRepresentable<TypeExpr>)

    /// A conformance constraint on a skolem.
    case conformance(
      lhs: SourceRepresentable<NameTypeExpr>,
      rhs: SourceRepresentable<TraitComposition>)

    /// A size constraint on a value parameter.
    case size(SourceRepresentable<Expr>)

  }

  /// The constraint expressions in the clause.
  public var constraints: [ConstraintExpr]

}
