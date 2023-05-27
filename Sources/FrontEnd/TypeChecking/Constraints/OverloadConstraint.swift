import Core
import Utils

/// A constraint specifying that a name expression refers to one of several declarations,
/// depending on its type.
struct OverloadConstraint: DisjunctiveConstraintProtocol, Hashable {

  /// The overloaded expression.
  let overloadedExpr: NameExpr.ID

  /// The type of `self.overloadedExpr`.
  private(set) var overloadedExprType: AnyType

  /// The overloaded candidates.
  private(set) var choices: [Predicate]

  let origin: ConstraintOrigin

  /// Creates an instance with the given properties.
  ///
  /// - Requires: `candidates.count >= 2`
  init(
    _ expr: NameExpr.ID,
    withType type: AnyType,
    refersToOneOf choices: [Predicate],
    origin: ConstraintOrigin
  ) {
    precondition(choices.count >= 2)

    // Insert an equality constraint in all candidates.
    self.choices = choices.map({ (c) -> Predicate in
      var a = c.constraints
      a.insert(EqualityConstraint(type, c.type, origin: origin))
      return .init(reference: c.reference, type: c.type, constraints: a, penalties: c.penalties)
    })

    self.overloadedExpr = expr
    self.overloadedExprType = type
    self.origin = origin
  }

  mutating func modifyTypes(_ transform: (AnyType) -> AnyType) {
    update(&overloadedExprType, with: transform)

    for i in 0 ..< choices.count {
      choices[i] = Predicate(
        reference: choices[i].reference,
        type: transform(choices[i].type),
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

  /// A candidate in an overload constraint.
  struct Predicate: DisjunctiveConstraintTerm, Hashable {

    /// The candidate reference.
    let reference: DeclReference

    /// The instantiated type the referred declaration.
    let type: AnyType

    /// The set of constraints associated with this choice.
    let constraints: ConstraintSet

    /// The penalties associated with this choice.
    let penalties: Int

  }

}

extension OverloadConstraint: CustomStringConvertible {

  var description: String {
    "\(overloadedExpr) ∈ {\(choices.descriptions())}"
  }

}

extension OverloadConstraint.Predicate: CustomStringConvertible {

  var description: String {
    "\(reference) => {\(list: constraints, joinedBy: " ∧ ")}:\(penalties)"
  }

}
