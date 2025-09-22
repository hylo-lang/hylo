import Utils

/// A constraint used to perform type checking or inference.
///
/// A constraint is a predicate over one or multiple types, including type variables, that must be
/// satisfied in order for a program to be well-typed. Constraints also server to infer implicit
/// type information from the structure of the program.
protocol Constraint: Sendable {

  /// The site from which a constraint originates and the reason why it was formed.
  var origin: ConstraintOrigin { get }

  /// Inserts the type variables that occur free in `self` into `s`.
  func collectOpenVariables(in s: inout Set<TypeVariable>)

  /// Applies `transform` on constituent types of `self`.
  mutating func modifyTypes(_ transform: (AnyType) -> AnyType)

  /// Hashes the salient features of `self` by feeding them into `hasher`.
  func hash(into hasher: inout Hasher)

  /// Returns whether `self` is equal to `other`.
  func equals<Other: Constraint>(_ other: Other) -> Bool

}

extension Constraint {

  /// Returns the type variables that occur free in `self`.
  var openVariables: Set<TypeVariable> {
    var result: Set<TypeVariable> = []
    collectOpenVariables(in: &result)
    return result
  }

  /// Returns a copy of `self` where constituent types have been transformed with `transform`.
  func modifyingTypes(_ transform: (AnyType) -> AnyType) -> Self {
    var copy = self
    copy.modifyTypes(transform)
    return copy
  }

}

extension Constraint where Self: Equatable {

  /// Returns whether `self` is equal to `other`.
  func equals<Other: Constraint>(_ other: Other) -> Bool {
    if let r = other as? Self {
      return self == r
    } else {
      return false
    }
  }

}
