import Utils

/// A type whose generic parameters have been substituted by variables along with the constraints
/// related to these variables.
struct InstantiatedType: Hashable {

  /// A type free of generic parameters.
  ///
  /// - Requires: `shape` does not contain any generic parameter, unless it is bound by an
  ///   existential type that's part of `shape`.
  let shape: AnyType

  /// The constraints related to the open variables in `shape`.
  let constraints: ConstraintSet

}
