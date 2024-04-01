/// The site from which a constraint originates and the reason why it was formed.
struct ConstraintOrigin: Hashable {

  /// The reason why a constraint was formed.
  enum Kind: Hashable {

    /// The constraint is caused by a type annotation.
    case annotation

    /// The constraint is caused by an expression being passed as an argument.
    case argument

    /// The constraint is caused by a binding reference.
    case binding

    /// The constraint is caused by a conditional expression.
    case branchMerge

    /// The constraint is caused by an expression being used as a callee.
    case callee

    /// The constraint is caused by a cast expression.
    case cast

    /// The constraint is caused by a discard statement.
    case discard

    /// The constraint is caused by an initialization or assignment.
    case initializationOrAssignment

    /// The constraint is caused by a binding initialization with a type hint.
    case initializationWithHint

    /// The constraint is caused by a binding initialization with a pattern.
    case initializationWithPattern

    /// The constraint is caused by optional binding.
    case optionalBinding

    /// The constraint is caused by the evaluation of a literal expression.
    case literal

    /// The constraint is caused by a member binding reference.
    case member

    /// The constraint is caused by some structural property of the AST.
    case structural

    /// The constraint is caused by a return statement.
    case `return`

    /// The constraint is caused by the use of an entity declared under a where clause.
    case whereClause

    /// The constraint is caused by a yield statement.
    case `yield`

    /// The constraint is caused by another constraint.
    ///
    /// - Warning: Do not use this kind outside of constraint solving. It is meant to be used by
    ///   a `ConstraintSystem` to keep track of dependencies.
    indirect case subordinate(parent: ConstraintOrigin)

  }

  /// The reason of the constraint.
  let kind: Kind

  /// The site from which the constraint originates.
  let site: SourceRange

  /// Creates a new instance with the given properties.
  init(_ kind: Kind, at site: SourceRange) {
    self.kind = kind
    self.site = site
  }

  /// Returns the parent of this instance if it has kind `.subordinate`.
  var parent: ConstraintOrigin? {
    if case .subordinate(let p) = kind {
      return p
    } else {
      return nil
    }
  }

  /// Returns a subordinate of this instance with given `id`.
  func subordinate() -> ConstraintOrigin {
    .init(.subordinate(parent: self), at: site)
  }

}

extension ConstraintOrigin: CustomStringConvertible {

  var description: String { .init(describing: kind) }

}
