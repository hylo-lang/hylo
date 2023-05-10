import Core
import Utils

/// A disjunction of two or more constraint sets.
struct DisjunctionConstraint: DisjunctiveConstraintProtocol, Hashable {

  /// The different choices in this disjunction.
  private(set) var choices: [Predicate]

  let origin: ConstraintOrigin

  /// Creates an instance with two or more minterms.
  ///
  /// - Requires: `choices.count >= 2`
  init(choices: [Predicate], origin: ConstraintOrigin) {
    precondition(choices.count >= 2)
    self.choices = choices
    self.origin = origin
  }

  mutating func modifyTypes(_ transform: (AnyType) -> AnyType) {
    for i in 0 ..< choices.count {
      choices[i] = Predicate(
        constraints: choices[i].constraints.reduce(
          into: [],
          { (cs, c) in
            var newConstraint = c
            newConstraint.modifyTypes(transform)
            cs.insert(newConstraint)
          }),
        penalties: choices[i].penalties)
    }
  }

  /// A collection of constraints in a disjunction.
  struct Predicate: DisjunctiveConstraintTerm, Hashable {

    /// Creates an instance having the given properties.
    init(constraints: ConstraintSet, penalties: Int) {
      self.constraints = constraints
      self.penalties = penalties
    }

    /// The constraints associated with this choice.
    let constraints: ConstraintSet

    /// The penalties associated with this choice.
    let penalties: Int

  }

}

extension DisjunctionConstraint: CustomStringConvertible {

  var description: String {
    choices.descriptions(joinedBy: " ∨ ")
  }

}

extension DisjunctionConstraint.Predicate: CustomStringConvertible {

  var description: String {
    "{\(list: constraints, joinedBy: " ∧ ")}:\(penalties)"
  }

}
