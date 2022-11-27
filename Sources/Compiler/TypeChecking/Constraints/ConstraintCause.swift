/// An object describing the cause of a constraint.
public struct ConstraintCause: Hashable {

  /// The kind of a constraint cause.
  public enum Kind {

    /// The constraint is caused by a type annotation.
    case annotation

    /// The constraint is caused by an expression being passed as an argument.
    case argument

    /// The constraint is caused by a binding reference.
    case binding

    /// The constraint is caused by an expression being used as a callee.
    case callee

    /// The constraint is caused by a cast expression.
    case cast

    /// The constraint is caused by an initialization.
    case initialization

    /// The constraint is caused by an initialization or assignment.
    case initializationOrAssignment

    /// The constraint is caused by the evaluation of a literal expression.
    case literal

    /// The constraint is caused by a member binding reference.
    case member

    /// The constraint is caused by some structural property of the AST.
    case structural

    /// The constraint is caused by a return statement.
    case `return`

    /// The constraint is caused by a yield statement.
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
