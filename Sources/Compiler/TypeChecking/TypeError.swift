/// An error encountered during type checking.
struct TypeError {

  /// The kind of a type error.
  enum Kind {

    /// The associated constraint was stale.
    case staleConstaint

  }

  /// The kind of the error.
  var kind: Kind

  /// The constraint that cause the type error.
  var cause: LocatableConstraint

}
