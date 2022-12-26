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

/// Creates a constraint, suitable for type inference, requiring `subtype` to be a subtype of
/// `supertype`.
///
/// - Warning: For inference purposes, the result of this function must be used in place of a raw
///   `SubtypingConstraint` or the type checker will get stuck.
public func inferenceConstraint(
  _ subtype: AnyType,
  isSubtypeOf supertype: AnyType,
  because cause: ConstraintCause
) -> DisjunctionConstraint {
  DisjunctionConstraint(
    choices: [
      .init(constraints: [EqualityConstraint(subtype, supertype, because: cause)], penalties: 0),
      .init(constraints: [SubtypingConstraint(subtype, supertype, because: cause)], penalties: 1),
    ],
    because: cause)
}
