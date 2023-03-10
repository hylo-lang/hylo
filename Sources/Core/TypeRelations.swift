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
    modify(&conformances[canonical(c.model), default: [:]]) { (traitToConformance) in
      modify(&traitToConformance[c.concept, default: []]) { (allConformances) in
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
    case let t as BoundGenericType:
      let base = canonical(t.base)
      let arguments = t.arguments.map({ (a) -> BoundGenericType.Argument in
        switch a {
        case .type(let a):
          return .type(canonical(a))
        case .value:
          fatalError("not implemented")
        }
      })
      return ^BoundGenericType(base, arguments: arguments)

    case let t as ExistentialType:
      return ^ExistentialType(
        traits: t.traits,
        constraints: ConstraintSet(t.constraints.map(canonical(_:))))

    case let t as MetatypeType:
      return ^MetatypeType(of: canonical(t.instance))

    case let t as SumType:
      return ^SumType(Set(t.elements.map(canonical(_:))))

    case let t as TupleType:
      return ^TupleType(
        t.elements.map({ (e) -> TupleType.Element in
          .init(label: e.label, type: canonical(e.type))
        }))

    case let t as TypeAliasType:
      return canonical(t.resolved.value)

    default:
      unreachable()
    }
  }

  /// Returns the canonical form of `constraint`.
  public func canonical(_ constraint: Constraint) -> Constraint {
    constraint.modifyingTypes(canonical(_:))
  }

}
