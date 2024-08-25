import Utils

/// An existential type, optionally bound by traits and constraints on associated types.
public struct ExistentialType: TypeProtocol {

  /// The interface of an existential type.
  public enum Interface: Hashable {

    /// The traits to which the witness is known to conform.
    case traits(Set<TraitType>)

    /// The declaration of the unparameterized generic type of the witness.
    case generic(AnyType)

    /// An unparameterized metatype.
    case metatype

    /// A set of flags describing recursive properties.
    public var flags: ValueFlags {
      switch self {
      case .traits(let ts):
        return .init(ts.map(\.flags))
      case .generic(let t):
        return t.flags
      case .metatype:
        return .init()
      }
    }

  }

  /// The interface of this type's instances.
  public let interface: Interface

  /// The constraints on the associated types of the witness.
  ///
  /// - Note: This set shall only contain equality and conformance constraints.
  public let constraints: Set<GenericConstraint>

  public let flags: ValueFlags

  /// Creates a new existential type bound by the given interface and constraints.
  public init(_ interface: Interface, constraints: Set<GenericConstraint>) {
    self.interface = interface
    self.constraints = constraints

    // TODO: Consider the flags of the types in the constraints?
    self.flags = interface.flags
  }

  /// Creates a new existential type bound by the given traits and constraints.
  public init(traits: Set<TraitType>, constraints: Set<GenericConstraint>) {
    self.init(.traits(traits), constraints: constraints)
  }

  /// Creates a new existential type bound by an unparameterized generic type and constraints.
  public init(unparameterized t: AnyType, constraints: Set<GenericConstraint>) {
    switch t.base {
    case is ProductType, is TypeAliasType:
      self.init(.generic(t), constraints: constraints)
    case is MetatypeType:
      self.init(.metatype, constraints: constraints)
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
