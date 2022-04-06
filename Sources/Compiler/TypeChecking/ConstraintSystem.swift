/// A set of equations describe relations between the types of a program.
struct ConstraintSystem {

  /// The stack of "fresh" constraints that are yet to be solved.
  var freshConstraints: [Constraint] = []

  /// The stack of "stale" constraints, which can't be solved before new bindings are inferred.
  var staleConstraints: [Constraint] = []

  /// Returns a copy of this constraint system with additional fresh constraint.
  func fork(inserting constraint: Constraint) -> ConstraintSystem {
    var newSystem = self
    newSystem.freshConstraints.append(constraint)
    return newSystem
  }

  /// Sorts the fresh stack of constraints so that the simplest ones appear at the top.
  mutating func sort() {
    freshConstraints.sort(by: { a, b in b.precedence < a.precedence })
  }

  /// "Refreshes" the stale constraints that directly depends the specified type variable.
  mutating func refresh(constraintsDependingOn tau: TypeVar) {
    let indices = staleConstraints.indices.filter({ index in
      staleConstraints[index].depends(on: tau)
    })
    staleConstraints.move(elementsIndexedBy: indices.reversed(), to: &freshConstraints)
  }

  /// Insert the specified constraint into the system.
  ///
  /// The constraint is inserted in the "fresh" stack.
  mutating func insert(_ constraint: Constraint) {
    freshConstraints.append(constraint)
  }

  /// Inserts a disjunction of weighted constraints into the system.
  ///
  /// - Parameters:
  ///   - choices: A sequence of pairs `(constraints, weight)` where `constraints` is a an array
  ///     with the constraints of a specific choice and `weight` is the associated penalty. If the
  ///     sequence contains a single choice, `constraints` are inserted into the system directly.
  ///     Otherwise, a disjunction is created. The method has no effect if `choices` is empty.
  ///   - locator: The locator of the disjunction.
  mutating func insert<S>(disjunctionWithWeights choices: S, locator: ConstraintLocator)
  where S: Sequence, S.Element == DisjunctionConstraint.Choice
  {
    let choices = Array(choices)

    if choices.count == 1 {
      freshConstraints.append(contentsOf: choices[0].0)
    } else if choices.count > 1 {
      let cons = DisjunctionConstraint(choices, locator: locator)
      freshConstraints.insert(cons, at: 0)
    }
  }

  /// Inserts a disjunction of constraints into the system.
  ///
  /// This method is an alias for `insert(disjunctionOfConstraintsWithWeights:)` where all choices
  /// have the same weight.
  mutating func insert<S>(disjunction sequence: S, locator: ConstraintLocator)
  where S: Sequence, S.Element == [Constraint]
  {
    let choices = sequence.map({ (constraints: $0, weight: 0) })
    insert(disjunctionWithWeights: choices, locator: locator)
  }

  /// Inserts a new relational constraint created from the specified prototype.
  ///
  /// - Parameters:
  ///   - prototype: The prototype of a constraint on an opened generic parameter.
  ///   - locator: A locator for the constraint.
  mutating func insert(
    prototype: GenericEnv.ConstraintPrototype,
    at locator: ConstraintLocator
  ) {
    insert(RelationalConstraint(prototype: prototype, at: locator))
  }

}

extension ConstraintSystem {

  /// Dumps a textual representation of the constraint system.
  func dump() {
    print("Fresh:")
    for constraint in freshConstraints { print("  ▶", constraint) }
    print("Stale:")
    for constraint in staleConstraints { print("  ▶", constraint) }
  }

}

extension Array {

  /// Moves the elements at the specified indices to another array.
  ///
  /// - Parameters:
  ///   - indices: The indices of the elements to transfer. `indices` sequence *must* be in reverse
  ///     order (i.e., from greatest to smallest).
  ///   - array: The array into which the elements are transferred.
  fileprivate mutating func move<S>(elementsIndexedBy indices: S, to array: inout [Element])
  where S: Sequence, S.Element == Index
  {
    for index in indices {
      array.append(remove(at: index))
    }
  }

}
