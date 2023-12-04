import Core

/// A set of predicates that is satisfied if at least one of them is satisfied.
protocol DisjunctiveConstraintProtocol: Constraint {

  /// The type of a specific choice in instances of this type.
  associatedtype Predicate: DisjunctiveConstraintTerm

  /// The predicates of this disjunctive constraints.
  var choices: [Predicate] { get }

}

/// A predicate in a disjunctive constraint.
protocol DisjunctiveConstraintTerm {

  /// The constituent constraints of the predicate.
  var constraints: ConstraintSet { get }

  /// The penalties associated with this predicate during constraint solving.
  var penalties: Int { get }

}

extension DisjunctiveConstraintProtocol {

  /// Inserts the type variables that occur free in `self` into `s`.
  func collectOpenVariables(in s: inout Set<TypeVariable>) {
    for p in choices {
      for c in p.constraints {
        c.collectOpenVariables(in: &s)
      }
    }
  }

}
