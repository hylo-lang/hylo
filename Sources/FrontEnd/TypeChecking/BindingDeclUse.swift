import Core

/// How a binding declaration is being used.
enum BindingDeclUse: Hashable {

  /// The declaration is used to introduce new bindings unconditionally.
  ///
  /// The pattern matches any possible value produced by the declaration's initializer, if present.
  case irrefutable

  /// The declaration is used as a condition.
  ///
  /// The pattern acts as a condition that is satisfied iff it matches the value of the
  /// declaration's initializer, which must be present.
  case condition

  /// The declaration is used as a filter.
  ///
  /// The pattern acts as a condition that is satisfied iff it matches an instance of the payload.
  case filter(matching: AnyType)

}
