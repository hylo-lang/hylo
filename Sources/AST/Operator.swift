import Basic

/// A prefix operator.
public enum PrefixOperator: String, CustomStringConvertible {

  /// The `+` operator.
  case plus   = "+"

  /// The `-` operator.
  case minus  = "-"

  /// The `!` operator.
  case bang   = "!"

  /// The `~` operator.
  case tilde  = "~"

  /// The `&` operator.
  case amp    = "&"

  public var description: String { rawValue }

}

/// An infix operator.
public enum InfixOperator: String, CustomStringConvertible {

  /// The copy operator `=`.
  case copy   = "="

  /// The `+` operator.
  case plus   = "+"

  /// The operator's precedence, as a numeric value.
  public var precedence: Int {
    switch self {
    case .copy: return 0
    case .plus: return 400
    }
  }

  /// The operator's associativity.
  public var associativity: Associativity? {
    switch self {
    case .copy: return .left
    case .plus: return .left
    }
  }

  /// An operator associativity.
  public enum Associativity {

    /// Left-associative (i.e., `a ◇ b ◇ c = (a ◇ b) ◇ c`).
    case left

    /// Right-associative (i.e., `a ◇ b ◇ c = a ◇ (b ◇ c)`).
    case right

  }

  public var description: String { rawValue }

}
