/// Information identifying a run-time argument of a function or subscript call.
public enum ArgumentResolutionResult: Hashable, Monotonic {

  /// The argument is the n-th expression in the syntax of the call.
  case explicit(Int)

  /// The argument is a an implicit declaration reference.
  case implicit(AnyDeclID)

  /// The argument is elided; the callee receive the parameter's default value.
  case defaulted

}
