import Utils

/// A type serving as a witness for `Constraint`s conformance to `Hashable`.
struct ConstraintHashableWitness: HashableWitness, Sendable {

  typealias Element = Constraint

  static func hash(_ constraint: Constraint, into hasher: inout Hasher) {
    constraint.hash(into: &hasher)
  }

  static func isEqual(_ left: Constraint, to right: Constraint) -> Bool {
    left.equals(right)
  }

}

typealias ConstraintSet = CustomWitnessedSet<ConstraintHashableWitness>
