import Utils

/// A type serving as a witness for `Constraint`s conformance to `Hashable`.
public struct ConstraintHashableWitness: HashableWitness {

  public typealias Element = Constraint

  public static func hash(_ constraint: Constraint, into hasher: inout Hasher) {
    constraint.hash(into: &hasher)
  }

  public static func isEqual(_ left: Constraint, to right: Constraint) -> Bool {
    left.equals(right)
  }

}

public typealias ConstraintSet = CustomWitnessedSet<ConstraintHashableWitness>
