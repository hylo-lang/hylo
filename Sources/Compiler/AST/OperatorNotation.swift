/// An operator notation.
public enum OperatorNotation: String {

  /// The infix notation.
  case infix

  /// The prefix notation.
  case prefix

  /// The postfix notation.
  case postfix

}

extension OperatorNotation: Hashable {}
