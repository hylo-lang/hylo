/// A receiver modifier.
public struct ReceiverModifier: MemberModifier {

  public enum Kind {

    case sink

    case `inout`

    case out

  }

  public var range: SourceRange?

  /// The kind of the modifier.
  public var kind: Kind

}

extension ReceiverModifier: CustomStringConvertible {

  public var description: String { "\(kind)" }

}
