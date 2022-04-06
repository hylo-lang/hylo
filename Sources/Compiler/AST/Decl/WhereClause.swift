import Utils

/// A where clause.
public struct WhereClause: SourceRepresentable {

  /// A constraint in a where clause.
  public enum Constraint {

    /// An equality constraint involving one or two skolems.
    case equality(lhs: TypeExpr, rhs: TypeExpr)

    /// A conformance constraint on a skolem.
    case conformance(lhs: NameTypeExpr, rhs: TraitComposition)

    /// A size constraint on a value parameter.
    case size(Expr)

  }

  public var range: SourceRange?

  /// The constraints in the clause.
  public var constraints: [Constraint]

}

extension WhereClause: CustomStringConvertible {

  public var description: String {
    if constraints.isEmpty {
      return "where _"
    } else {
      return "where " + String.joining(constraints, separator: ", ")
    }
  }

}

extension WhereClause.Constraint: CustomStringConvertible {

  public var description: String {
    switch self {
    case let .equality(lhs: lhs, rhs: rhs):
      return "\(lhs) == \(rhs)"
    case let .conformance(lhs: lhs, rhs: rhs):
      return "\(lhs) : \(rhs)"
    case let .size(expr):
      return "\(expr)"
    }
  }

}
