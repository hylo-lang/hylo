import Utils

/// A collection of relations between the types of a program.
public struct TypeRelations {

  /// A set of conformances represented to answer "does A conform to T in S" efficiently.
  public private(set) var conformances: [AnyType: [TraitType: [Conformance]]] = [:]

  /// Creates an instance.
  public init() {}

  /// Inserts `c` in the conformance relation if not already present, using `p` to test whether
  /// scopes overlap.
  ///
  /// - Returns: `(true, c)` if no conformance describing how `c.model` satisfies `c.trait` in a
  ///   scope overlapping with `c.scope` was already contained in the relation. Otherwise, returns
  ///   `(false, other)`, where `other` is the existing conformance.
  @discardableResult
  public mutating func insert<P: Program>(
    _ c: Conformance,
    testingContainmentWith p: P
  ) -> (inserted: Bool, conformanceAfterInsert: Conformance) {
    modifying(&conformances[canonical(c.model), default: [:]]) { (traitToConformance) in
      modifying(&traitToConformance[c.concept, default: []]) { (allConformances) in
        if let x = allConformances.first(where: { p.areOverlapping($0.scope, c.scope) }) {
          return (false, x)
        } else {
          allConformances.append(c)
          return (true, c)
        }
      }
    }
  }

  /// Returns whether `lhs` is equivalent to `rhs`.
  public func areEquivalent(_ lhs: AnyType, _ rhs: AnyType) -> Bool {
    canonical(lhs) == canonical(rhs)
  }

  /// Returns whether `lhs` is a strict subtype of `rhs`.
  public func isStrictSubtype(_ lhs: AnyType, _ rhs: AnyType) -> Bool {
    // TODO: Implement me
    return false
  }

  /// Returns the canonical form of `t`.
  public func canonical(_ type: AnyType) -> AnyType {
    if type[.isCanonical] { return type }

    switch type.base {
    case let t as TypeAliasType:
      return canonical(t.resolved.value)
    default:
      return type.transformParts({ (t) in .stepOver(canonical(t)) })
    }
  }

  /// Returns the canonical form of `constraint`.
  public func canonical(_ constraint: Constraint) -> Constraint {
    constraint.modifyingTypes(canonical(_:))
  }

}
