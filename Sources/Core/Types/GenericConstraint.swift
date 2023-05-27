/// A constraint on generic type and value parameters.
public struct GenericConstraint: Hashable {

  /// The value of a generic constraint.
  public enum Value: Hashable {

    /// A constraint specifying that `lhs` is equal to `rhs`, defined at `site`.
    case equality(_ lhs: AnyType, _ rhs: AnyType)

    /// A constraint specifying that `lhs` is an instance of `rhs`, defined at `site`.
    case instance(_ lhs: AnyExprID, _ rhs: AnyType)

    /// A constraint specifying that `lhs` conforms to the trais in `rhs`, defined at `site`.
    case conformance(_ lhs: AnyType, _ rhs: Set<TraitType>)

    /// A constraint specifying that the payload evaluates to `true`.
    case predicate(AnyExprID)

  }

  /// The value of the constraint.
  public let value: Value

  /// The site from which `self` was parsed.
  public let site: SourceRange

  /// Creates an instance with given properties.
  public init(_ value: Value, at site: SourceRange) {
    self.value = value
    self.site = site
  }

}
