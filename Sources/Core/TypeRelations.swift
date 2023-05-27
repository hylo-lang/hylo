import Utils

/// A collection of relations between the types of a program.
public struct TypeRelations {

  /// A set of conformances represented to answer "does A conform to T in S" efficiently.
  public typealias ConformanceSet = [AnyType: ConformanceTable]

  /// A table from trait to its conformances for a given type.
  public typealias ConformanceTable = [TraitType: [Conformance]]

  /// The conformances in the program.
  public private(set) var conformances: ConformanceSet = [:]

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
    case let t as TypeAliasType:
      return canonical(t.resolved.value)
    default:
      return type.transformParts({ (t) in .stepOver(canonical(t)) })
    }
  }

  /// Returns `arguments` with all types replaced by their canonical form.
  public func canonical(_ arguments: GenericArguments) -> GenericArguments {
    arguments.mapValues { (v) in
      (v as? AnyType).map(canonical(_:)) ?? v
    }
  }

  /// Returns a copy of `generic` monomorphized for the given `arguments`.
  ///
  /// This method has no effect if `arguments` is empty.
  public func monomorphize(_ generic: AnyType, for arguments: GenericArguments) -> AnyType {
    return arguments.isEmpty ? generic : generic.transform(transform(_:))

    /// Returns how to specialize `t`.
    func transform(_ t: AnyType) -> TypeTransformAction {
      switch t.base {
      case let u as AssociatedTypeType:
        return transform(u)
      case let u as BoundGenericType:
        return transform(u)
      case let u as GenericTypeParameterType:
        return .stepOver((arguments[u.decl] as? AnyType) ?? .error)
      case let u as SkolemType:
        return transform(u)
      default:
        return .stepInto(t)
      }
    }

    /// Returns how to monomorphize `t`.
    func transform(_ t: AssociatedTypeType) -> TypeTransformAction {
      // Note: requires either the method to accept type and scope as parameters, or to have
      // associated types remember the trait that introduced them.
      fatalError("not implemented")
    }

    /// Returns how to monomorphize `t`.
    func transform(_ t: BoundGenericType) -> TypeTransformAction {
      let updatedArguments = t.arguments.mapValues { (v) -> any CompileTimeValue in
        if let w = v as? AnyType {
          return monomorphize(w, for: arguments)
        } else {
          return v
        }
      }
      return .stepOver(^BoundGenericType(t.base, arguments: updatedArguments))
    }

    /// Returns how to monomorphize `t`.
    func transform(_ t: SkolemType) -> TypeTransformAction {
      .stepOver(monomorphize(t.base, for: arguments))
    }
  }

}
