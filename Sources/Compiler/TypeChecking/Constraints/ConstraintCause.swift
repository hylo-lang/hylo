/// An object describing the cause of a constraint.
public struct ConstraintCause: Hashable {

  /// The kind of a constraint cause.
  public enum Kind {

    case annotation

    case assignment

    case callArgument

    case callee

    case cast

    case initialization

    case literal

    case member

    case reference

    case structural

    case `return`

    case `yield`

  }

  /// The kind this cause.
  public let kind: Kind

  /// The source range from which the constraint originates, if any.
  public let origin: SourceRange?

  /// Creates a new instance with the given properties.
  public init(_ kind: Kind, at origin: SourceRange?) {
    self.kind = kind
    self.origin = origin
  }

}
