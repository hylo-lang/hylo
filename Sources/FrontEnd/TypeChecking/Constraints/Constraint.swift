/// A type consraint used to perform type checking or inference.
///
/// A constraint is a predicate over one or multiple types, including type variables, that must be
/// satisfied in order for a program to be well-typed. Constraints also server to infer implicit
/// type information from the structure of the program.
public protocol Constraint {

  /// The cause of the constraint.
  var cause: ConstraintCause { get set }

  /// Applies `modify` on the types that are part of `self`.
  mutating func modifyTypes(_ modify: (inout AnyType) -> Void)

  /// Returns whether the constraint depends on the specified variable.
  func depends(on variable: TypeVariable) -> Bool

  /// Hashes the salient features of `self` by feeding them into `hasher`.
  func hash(into hasher: inout Hasher)

  /// Returns whether `self` is equal to `other`.
  func equals<Other: Constraint>(_ other: Other) -> Bool

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

/// Creates a constraint requiring `l` to be either equal to or a subtype of `r`.
func equalityOrSubtypingConstraint(
  _ l: AnyType,
  _ r: AnyType,
  because cause: ConstraintCause
) -> DisjunctionConstraint {
  DisjunctionConstraint(
    choices: [
      .init(constraints: [EqualityConstraint(l, r, because: cause)], penalties: 0),
      .init(constraints: [SubtypingConstraint(l, r, because: cause)], penalties: 1),
    ],
    because: cause)
}

/// Creates a constraint requiring `subject` to be either equal to `defaultType` or be expressible
/// by literals associated with `trait`.
///
/// - Requires: `trait` must be a one of the `ExpressibleByXXXLiteral` traits from the core library
///   and `defaultType` must be the corresponding core type.
func expressibleByLiteralConstraint(
  _ subject: AnyType,
  trait: TraitType,
  defaultType: AnyType,
  because cause: ConstraintCause
) -> DisjunctionConstraint {
  return DisjunctionConstraint(
    choices: [
      .init(
        constraints: [EqualityConstraint(subject, defaultType, because: cause)],
        penalties: 0),
      .init(
        constraints: [ConformanceConstraint(subject, conformsTo: [trait], because: cause)],
        penalties: 1),
    ],
    because: cause)
}
