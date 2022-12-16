import Utils

/// A disjunction of two or more constraint sets.
///
/// Each set is associated with a penalty to represent the preferred alternatives.
public struct DisjunctionConstraint: Constraint, Hashable {

  /// A collection of constraints in a disjunction.
  public struct Choice: Hashable {
    
    /// Creates an instance having the given properties.
    public init(constraints: ConstraintSet, penalties: Int) {
      self.constraints = constraints
      self.penalties = penalties
    }

    /// The constraints.
    public let constraints: ConstraintSet

    /// The penalties associated with the set.
    public let penalties: Int

  }

  /// The choices of the disjunction.
  public private(set) var choices: [Choice]

  public var cause: ConstraintCause

  /// Creates an instance with two or more minterms.
  ///
  /// - Requires: `choices.count >= 2`
  public init(choices: [Choice], because cause: ConstraintCause) {
    precondition(choices.count >= 2)
    self.choices = choices
    self.cause = cause
  }

  public mutating func modifyTypes(_ modify: (inout AnyType) -> Void) {
    for i in 0 ..< choices.count {
      choices[i] = Choice(
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
    for m in choices {
      for c in m.constraints {
        if c.depends(on: variable) { return true }
      }
    }
    return false
  }

}

extension DisjunctionConstraint: CustomStringConvertible {

  public var description: String {
    choices.descriptions(joinedBy: " ∨ ")
  }

}

extension DisjunctionConstraint.Choice: CustomStringConvertible {

  public var description: String {
    "{\(constraints.descriptions(joinedBy: " ∧ "))}:\(penalties)"
  }

}
