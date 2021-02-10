import AST

/// A set of equations describe relations between the types of a program.
struct ConstraintSystem {

  /// The stack of "fresh" constraints that are yet to be solved.
  var freshConstraints: [Constraint] = []

  /// The stack of "stale" constraints, which can't be solved before new bindings are inferred.
  var staleConstraints: [Constraint] = []

  /// Returns a copy of this constraint system, with the additional constraint.
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
  /// - Parameter choices: A sequence of pairs of a constraint with an associated weight. If it
  ///   contains only a choice, then the constraint is inserted "as-is" into the system. Otherwise,
  ///   a disjunction is created The method has no effect if `choices` is empty.
  @discardableResult
  mutating func insert<S>(disjunctionOfConstraintsWithWeights choices: S) -> Constraint?
  where S: Sequence, S.Element == (Constraint, Int)
  {
    let choices = Array(choices)

    guard !choices.isEmpty else { return nil }
    guard choices.count > 1 else {
      freshConstraints.append(choices[0].0)
      return choices[0].0
    }

    let cons = DisjunctionConstraint(choices)
    freshConstraints.insert(cons, at: 0)
    return cons
  }

  /// Inserts a disjunction of constraints into the system.
  ///
  /// This is an alias for `insert(disjunctionOfConstraintsWithWeights:)` where all choices have
  /// the same weight.
  @discardableResult
  mutating func insert<S>(disjunction sequence: S) -> Constraint?
  where S: Sequence, S.Element == Constraint
  {
    return insert(disjunctionOfConstraintsWithWeights: sequence.map({ ($0, 0) }))
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
