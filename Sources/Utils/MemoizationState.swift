/// The state of a memoization cache for a particular input.
public enum MemoizationState<Result> {

  /// The result is being computed.
  case inProgress

  /// The result has been computed.
  case done(Result)

}

extension MemoizationState: Equatable where Result: Equatable {}

extension MemoizationState: Hashable where Result: Hashable {}
