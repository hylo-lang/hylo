import Utils

/// A type consraint used to perform type checking or inference.
///
/// A constraint is a predicate over one or multiple types, including type variables, that must be
/// satisfied in order for a program to be well-typed. Constraints also server to infer implicit
/// type information from the structure of the program.
public protocol Constraint {

  /// The cause of the constraint.
  var cause: ConstraintCause { get }

  /// Applies `transform` on constituent types of `self`.
  mutating func modifyTypes(_ transform: (AnyType) -> AnyType)

  /// Hashes the salient features of `self` by feeding them into `hasher`.
  func hash(into hasher: inout Hasher)

  /// Returns whether `self` is equal to `other`.
  func equals<Other: Constraint>(_ other: Other) -> Bool

}

extension Constraint {

  /// Returns a copy of `self` where constituent types have been transformed with `transform`.
  public func modifyingTypes(_ transform: (AnyType) -> AnyType) -> Self {
    var copy = self
    copy.modifyTypes(transform)
    return copy
  }

  /// Returns whether `self` is heuristically simpler to solve than `other`.
  public func simpler(than other: Constraint) -> Bool {
    switch self {
    case is EqualityConstraint:
      return !(other is EqualityConstraint)

    case let l as ForkingConstraint:
      if let r = other as? ForkingConstraint {
        return l.forks < r.forks
      } else {
        return false
      }

    default:
      return false
    }
  }

}

extension Constraint where Self: Equatable {

  /// Returns whether `self` is equal to `other`.
  public func equals<Other: Constraint>(_ other: Other) -> Bool {
    if let r = other as? Self {
      return self == r
    } else {
      return false
    }
  }

}

/// A type constraint that may cause forking constraint solving.
private protocol ForkingConstraint {

  var forks: Int { get }

}

extension DisjunctionConstraint: ForkingConstraint {

  var forks: Int { choices.count }

}

extension OverloadConstraint: ForkingConstraint {

  var forks: Int { choices.count }

}
