import Utils

/// A disjunction of two or more constraint sets.
///
/// Each set is associated with a penalty to represent the preferred alternatives.
struct DisjunctionConstraint: Constraint, Hashable {

  /// A collection of constraints in a disjunction.
  struct Choice: Hashable {

    /// The constraints.
    let constraints: ConstraintSet

    /// The penalties associated with the set.
    let penalties: Int

  }

  /// The choices of the disjunction.
  private(set) var choices: [Choice]

  var cause: ConstraintCause

  /// Creates an instance with two or more minterms.
  ///
  /// - Requires: `choices.count >= 2`
  init(choices: [Choice], because cause: ConstraintCause) {
    precondition(choices.count >= 2)
    self.choices = choices
    self.cause = cause
  }

  mutating func modifyTypes(_ modify: (inout AnyType) -> Void) {
    for i in 0..<choices.count {
      choices[i] = Choice(
        constraints: choices[i].constraints.reduce(
          into: [],
          { (cs, c) in
            var newConstraint = c
            newConstraint.modifyTypes(modify)
            cs.insert(newConstraint)
          }), penalties: choices[i].penalties)
    }
  }

  func depends(on variable: TypeVariable) -> Bool {
    for m in choices {
      for c in m.constraints {
        if c.depends(on: variable) { return true }
      }
    }
    return false
  }

}

extension DisjunctionConstraint: CustomStringConvertible {

  var description: String {
    choices.descriptions(joinedBy: " ∨ ")
  }

}

extension DisjunctionConstraint.Choice: CustomStringConvertible {

  var description: String {
    "{\(constraints.descriptions(joinedBy: " ∧ "))}:\(penalties)"
  }

}
