import Utils

/// A constraint specifying that a name expression refers to one of several declarations,
/// depending on its type.
public struct OverloadConstraint: DisjunctiveConstraintProtocol, Hashable {

  /// The overloaded expression.
  public let overloadedExpr: NameExpr.ID

  /// The type of `self.overloadedExpr`.
  public private(set) var overloadedExprType: AnyType

  /// The overloaded candidates.
  public private(set) var choices: [Predicate]

  public let origin: ConstraintOrigin

  /// Creates an instance with the given properties.
  ///
  /// - Requires: `candidates.count >= 2`
  public init(
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

  public mutating func modifyTypes(_ transform: (AnyType) -> AnyType) {
    assign(&overloadedExprType, to: transform)

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
  public struct Predicate: DisjunctiveConstraintTerm, Hashable {

    /// Creates an instance having the given properties.
    public init(reference: DeclRef, type: AnyType, constraints: ConstraintSet, penalties: Int) {
      self.reference = reference
      self.type = type
      self.constraints = constraints
      self.penalties = penalties
    }

    /// The candidate reference.
    public let reference: DeclRef

    /// The instantiated type the referred declaration.
    public let type: AnyType

    /// The set of constraints associated with this choice.
    public let constraints: ConstraintSet

    /// The penalties associated with this choice.
    public let penalties: Int

  }

}

extension OverloadConstraint: CustomStringConvertible {

  public var description: String {
    "\(overloadedExpr) ∈ {\(choices.descriptions())}"
  }

}

extension OverloadConstraint.Predicate: CustomStringConvertible {

  public var description: String {
    "\(reference) => {\(list: constraints, joinedBy: " ∧ ")}:\(penalties)"
  }

}
