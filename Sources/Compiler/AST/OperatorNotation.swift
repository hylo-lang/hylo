/// An operator notation.
public enum OperatorNotation {

  /// The infix notation.
  case infix

  /// The prefix notation.
  case prefix

  /// The suffix notation.
  case suffix

}

extension OperatorNotation: Hashable {}
