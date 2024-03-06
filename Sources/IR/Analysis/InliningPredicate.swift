/// A predicate testing whether inlining should apply.
public enum InliningPredicate {

  /// The function is inlined iff it has no control flow.
  case hasNoControlFlow

  /// Returns `true` iff `f`, which is defined in `m`, satisfies `self`.
  func callAsFunction(_ f: Function.ID, definedIn m: Module) -> Bool {
    switch self {
    case .hasNoControlFlow:
      return m[f].blocks.count == 1
    }
  }

}
