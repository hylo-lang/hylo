import Utils

/// An existential type, optionally bound by traits and constraints on associated types.
public struct ExistentialType: TypeProtocol {

  /// The interface of an existential type.
  public enum Interface: Hashable {

    /// The traits to which the witness is known to conform.
    case traits(Set<TraitType>)

    /// The unparameterized generic type of the witness.
    case generic(AnyType)

  }

  /// The interface of this type's instances.
  public let interface: Interface

  /// The constraints on the associated types of the witness.
  ///
  /// - Note: This set shall only contain equality and conformance constraints.
  public let constraints: ConstraintSet

  public let flags: TypeFlags

  /// Creates a new existential type bound by the given traits and constraints.
  public init(traits: Set<TraitType>, constraints: ConstraintSet) {
    for c in constraints {
      precondition(
        (c is EqualityConstraint) || (c is ConformanceConstraint),
        "type may only be constrained by equality or conformance")
    }

    self.interface = .traits(traits)
    self.constraints = constraints

    // FIXME: Consider the types in the cosntraints?
    self.flags = traits.reduce(into: TypeFlags.isCanonical, { (a, b) in a.merge(b.flags) })
  }

  /// Creates a new existential type bound by an unparameterized generic type and constraints.
  public init(unparameterized generic: AnyType, constraints: ConstraintSet) {
    precondition(isValidInterface(generic))
    for c in constraints {
      precondition(
        (c is EqualityConstraint) || (c is ConformanceConstraint),
        "type may only be constrained by equality or conformance")
    }

    self.interface = .generic(generic)
    self.constraints = constraints

    // FIXME: Consider the types in the cosntraints?
    self.flags = generic.flags
  }

}

extension ExistentialType: CustomStringConvertible {

  public var description: String {
    let i: String

    switch interface {
    case .traits(let traits):
      if traits.isEmpty && constraints.isEmpty {
        return "Any"
      } else {
        i = "\(list: traits, joinedBy: " & ")"
      }

    case .generic(let t):
      i = .init(describing: t)
    }

    if constraints.isEmpty {
      return "any \(i)"
    } else {
      return "any \(i) where \(list: constraints)"
    }
  }

}

/// Returns `true` iff `t` is a valid interface for an existential type.
private func isValidInterface(_ t: AnyType) -> Bool {
  switch t.base {
  case is ProductType, is TypeAliasType:
    return t[.hasGenericTypeParameter] || t[.hasGenericValueParameter]
  default:
    return false
  }
}
