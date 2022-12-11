import Utils

/// A constraint specifying that a name expression refers to one of several declarations,
/// depending on its type.
struct OverloadConstraint: Constraint, Hashable {

  /// A candidate in an overload constraint.
  struct Candidate: Hashable {

    /// The candidate reference.
    let reference: DeclRef

    /// The contextualized type the referred declaration.
    let type: AnyType

    /// The set of constraints associated with the reference.
    let constraints: ConstraintSet

    /// The penalties associated with the candidate.
    let penalties: Int

  }

  /// The overloaded expression.
  let overloadedExpr: NodeID<NameExpr>

  /// The type of `overloadedExpr`.
  private(set) var overloadedExprType: AnyType

  /// The choices of the disjunction.
  private(set) var choices: [Candidate]

  var cause: ConstraintCause

  /// Creates an instance with the given properties.
  ///
  /// - Requires: `candidates.count >= 2`
  init(
    _ expr: NodeID<NameExpr>, withType type: AnyType, refersToOneOf choices: [Candidate],
    because cause: ConstraintCause
  ) {
    precondition(choices.count >= 2)
    self.overloadedExpr = expr
    self.overloadedExprType = type
    self.choices = choices
    self.cause = cause
  }

  mutating func modifyTypes(_ modify: (inout AnyType) -> Void) {
    modify(&overloadedExprType)

    for i in 0..<choices.count {
      var newType = choices[i].type
      modify(&newType)

      choices[i] = Candidate(
        reference: choices[i].reference, type: newType,
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
    overloadedExprType == variable
  }

}

extension OverloadConstraint: CustomStringConvertible {

  var description: String {
    "\(overloadedExpr):\(overloadedExprType) ∈ {\(choices.descriptions())}"
  }

}

extension OverloadConstraint.Candidate: CustomStringConvertible {

  var description: String {
    "\(reference):\(type) => {\(constraints.descriptions(joinedBy: " ∧ "))}:\(penalties)"
  }

}
