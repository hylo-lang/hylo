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

  /// The AST node to which the constraint is attached, if any.
  public let node: AnyNodeID?

  /// The source range from which the constraint originates, if any.
  public let origin: SourceRange?

  /// Creates a new instance with the given properties.
  public init(kind: Kind, node: AnyNodeID?, origin: SourceRange?) {
    self.kind = kind
    self.node = node
    self.origin = origin
  }

}
