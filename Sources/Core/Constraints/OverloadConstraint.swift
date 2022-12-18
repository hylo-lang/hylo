import Utils

/// A constraint specifying that a name expression refers to one of several declarations,
/// depending on its type.
public struct OverloadConstraint: Constraint, Hashable {

  /// A candidate in an overload constraint.
  public struct Candidate: Hashable {
    
    /// Creates an instance having the given properties.
    public init(reference: DeclRef, type: AnyType, constraints: ConstraintSet, penalties: Int) {
      self.reference = reference
      self.type = type
      self.constraints = constraints
      self.penalties = penalties
    }
  
    /// The candidate reference.
    public let reference: DeclRef

    /// The contextualized type the referred declaration.
    public let type: AnyType

    /// The set of constraints associated with this choice.
    public let constraints: ConstraintSet

    /// The penalties associated with this choice.
    public let penalties: Int

  }

  /// The overloaded expression.
  public let overloadedExpr: NodeID<NameExpr>

  /// The type of `overloadedExpr`.
  public private(set) var overloadedExprType: AnyType

  /// The choices of the disjunction.
  public private(set) var choices: [Candidate]

  public let cause: ConstraintCause

  /// Creates an instance with the given properties.
  ///
  /// - Requires: `candidates.count >= 2`
  public init(
    _ expr: NodeID<NameExpr>,
    withType type: AnyType,
    refersToOneOf choices: [Candidate],
    because cause: ConstraintCause
  ) {
    precondition(choices.count >= 2)

    // Insert an equality constraint in all candidates.
    self.choices = choices.map({ (c) -> Candidate in
      var constraints = c.constraints
      constraints.insert(EqualityConstraint(type, c.type, because: cause))
      return .init(
        reference: c.reference, type: c.type, constraints: constraints, penalties: c.penalties)
    })

    self.overloadedExpr = expr
    self.overloadedExprType = type
    self.cause = cause
  }

  public mutating func modifyTypes(_ modify: (inout AnyType) -> Void) {
    modify(&overloadedExprType)

    for i in 0 ..< choices.count {
      var newType = choices[i].type
      modify(&newType)

      choices[i] = Candidate(
        reference: choices[i].reference,
        type: newType,
        constraints: choices[i].constraints.reduce(
          into: [],
          { (cs, c) in
            var newConstraint = c
            newConstraint.modifyTypes(modify)
            cs.insert(newConstraint)
          }),
        penalties: choices[i].penalties)
    }
  }

  public func depends(on variable: TypeVariable) -> Bool {
    overloadedExprType == variable
  }

}

extension OverloadConstraint: CustomStringConvertible {

  public var description: String {
    "\(overloadedExpr):\(overloadedExprType) ∈ {\(choices.descriptions())}"
  }

}

extension OverloadConstraint.Candidate: CustomStringConvertible {

  public var description: String {
    "\(reference):\(type) => {\(constraints.descriptions(joinedBy: " ∧ "))}:\(penalties)"
  }

}
