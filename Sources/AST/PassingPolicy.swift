/// A parameter passing policy.
public enum PassingPolicy {

  /// An immutable value that is guaranteed to be live over the entire scope of the function.
  case local

  /// A mutable borrowed value that is guaranteed unique over the entire scope of the function.
  case `inout`

  /// An immutable value whose ownership has been transferred from the caller to the callee.
  case consuming

  /// A mutable value whose ownership has been transferred from the caller to the callee.
  case consumingMutable

}
