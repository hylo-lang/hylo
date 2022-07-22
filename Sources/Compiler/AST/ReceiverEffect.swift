/// The effect of a call operator on the callee's receiver.
public enum ReceiverEffect: Hashable {

  /// The receiver is modified.
  case `inout`

  /// The receiver is consumed.
  case sink

  /// The receiver is yielded.
  case yielded

}
