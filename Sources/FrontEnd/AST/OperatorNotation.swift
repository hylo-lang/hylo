/// An operator notation.
public enum OperatorNotation: UInt8, Codable, Sendable {

  /// The infix notation.
  case infix

  /// The prefix notation.
  case prefix

  /// The postfix notation.
  case postfix

}
