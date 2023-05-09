import Utils

/// An existential type, optionally bound by traits and constraints on associated types.
public struct ExistentialType: TypeProtocol {

  /// The interface of an existential type.
  public enum Interface: Hashable {

    /// The traits to which the witness is known to conform.
    case traits(Set<TraitType>)

    /// The declaration of the unparameterized generic type of the witness.
    case generic(AnyDeclID)

    /// An unparameterized metatype.
    case metatype

  }

  /// The interface of this type's instances.
  public let interface: Interface

  /// The constraints on the associated types of the witness.
  ///
  /// - Note: This set shall only contain equality and conformance constraints.
  public let constraints: ConstraintSet

  /// A set of flags describing recursive properties.
  public let flags: TypeFlags

  /// Creates a new existential type bound by the given traits and constraints.
  public init(traits: Set<TraitType>, constraints: ConstraintSet) {
    // TODO: Consider the flags of the types in the cosntraints?
    for c in constraints {
      precondition(
        (c is EqualityConstraint) || (c is ConformanceConstraint),
        "type may only be constrained by equality or conformance")
    }

    self.interface = .traits(traits)
    self.constraints = constraints
    self.flags = traits.reduce(into: TypeFlags.isCanonical, { (a, b) in a.merge(b.flags) })
  }

  /// Creates a new existential type bound by an unparameterized generic type and constraints.
  public init(unparameterized t: AnyType, constraints: ConstraintSet) {
    // TODO: Consider the flags of the types in the cosntraints?
    for c in constraints {
      precondition(
        (c is EqualityConstraint) || (c is ConformanceConstraint),
        "type may only be constrained by equality or conformance")
    }
    self.constraints = constraints

    switch t.base {
    case let u as ProductType:
      self.interface = .generic(AnyDeclID(u.decl))
      self.flags = t.flags
    case let u as TypeAliasType:
      self.interface = .generic(AnyDeclID(u.decl))
      self.flags = t.flags
    case is MetatypeType:
      self.interface = .metatype
      self.flags = .isCanonical
    default:
      preconditionFailure()
    }
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

    case .metatype:
      i = "Metatype"
    }

    if constraints.isEmpty {
      return "any \(i)"
    } else {
      return "any \(i) where \(list: constraints)"
    }
  }

}
