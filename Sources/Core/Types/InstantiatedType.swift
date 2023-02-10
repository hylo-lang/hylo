import Utils

/// A type whose generic parameters have been substituted by variables along with the constraints
/// related to these variables.
public struct InstantiatedType: Hashable {

  /// A type free of generic parameters.
  public let shape: AnyType

  /// The constraints related to the open variables in `shape`.
  public let constraints: ConstraintSet

  /// Creates an instance with the given properties.
  ///
  /// - Requires: `shape` does not contain any generic parameter, unless it is bound by an
  ///   existential type that's part of `shape`.
  public init(shape: AnyType, constraints: ConstraintSet) {
    self.shape = shape
    self.constraints = constraints
  }

}
