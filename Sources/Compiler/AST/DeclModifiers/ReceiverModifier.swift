/// A member modifier.
public enum MemberModifier: Hashable {

  /// The kind of a receiver modifier.
  public enum Receiver: Hashable {

    case sink

    case `inout`

    case out

  }

  /// A receiver modifier.
  case receiver(Receiver)

  /// The `static` member modifier.
  case `static`

}
