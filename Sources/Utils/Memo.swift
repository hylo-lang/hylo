/// The state of memoization of a computation, including an "in progress" state that allows us to
/// detect cycles.
public enum Memo<T: Equatable>: Equatable {

  case inProgress, computed(T)

  /// The payload of the `.computed` case, if any, or `nil` if `self == .inProgress`.
  public var computed: T? {
    if case let .computed(x) = self {
      return x
    } else {
      return nil
    }
  }

}

extension Memo: Hashable where T: Hashable {}
