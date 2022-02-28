/// A prefix operator.
public enum PrefixOperator: String, CustomStringConvertible {

  /// The `+` operator.
  case plus     = "+"

  /// The `-` operator.
  case minus    = "-"

  /// The `!` operator.
  case bang     = "!"

  /// The `~` operator.
  case tilde    = "~"

  /// The `&` operator.
  case amp      = "&"

  /// The `async` operator.
  case async    = "async"

  /// The `await` operator.
  case await    = "await"

  public var description: String { rawValue }

}

/// An infix operator.
public enum InfixOperator: String, CustomStringConvertible {

  /// An operator associativity.
  public enum Associativity {

    /// Left-associative (i.e., `a ◇ b ◇ c = (a ◇ b) ◇ c`).
    case left

    /// Right-associative (i.e., `a ◇ b ◇ c = a ◇ (b ◇ c)`).
    case right

  }

  /// The copy operator `=`.
  case copy     = "="

  /// The `+` operator.
  case plus     = "+"

  /// The `-` operator.
  case minus    = "-"

  /// The `*` operator.
  case star     = "*"

  /// The `/` operator.
  case slash    = "/"

  /// The `%` operator.
  case percent  = "%"

  /// The operator's precedence, as a numeric value.
  public var precedence: Int {
    switch self {
    case .copy    : return 0
    case .plus    : return 400
    case .minus   : return 400
    case .star    : return 800
    case .slash   : return 800
    case .percent : return 800
    }
  }

  /// The operator's associativity.
  public var associativity: Associativity? {
    switch self {
    case .copy    : return .left
    case .plus    : return .left
    case .minus   : return .left
    case .star    : return .left
    case .slash   : return .left
    case .percent : return .left
    }
  }

  public var description: String { rawValue }

}

/// The precedence group of an operator.
public enum PrecedenceGroup: String {

  /// An operator associativity.
  public enum Associativity {

    /// Left-associative (i.e., `a ◇ b ◇ c = (a ◇ b) ◇ c`).
    case left

    /// Right-associative (i.e., `a ◇ b ◇ c = a ◇ (b ◇ c)`).
    case right

  }

  case assignment
  case logicalDisjunction
  case logicalConjunction
  case identifier
  case comparison
  case nilCoalescing
  case casting
  case addition
  case multiplication
  case shifting
  case exponentiation

  /// Returns the precedence group of the specified (non-identifier) operator.
  public init?(for oper: String) {
    switch oper {
    case "=", "+=", "-=", "*=", "/=", "%=", "<<=", ">>=", "&=", "|=", "^=":
      self = .assignment

    case "||":
      self = .logicalDisjunction

    case "&&":
      self = .logicalConjunction

    case "<", "<=", ">", ">=", "==", "!=", "~=":
      self = .comparison

    case "??", "!!":
      self = .nilCoalescing

    case "is", "as", "as!":
      self = .casting

    case "+", "-", "|", "^", "+!", "-!":
      self = .addition

    case "*", "/", "%", "&", "*!":
      self = .multiplication

    case "<<", ">>", "<<!", ">>!":
      self = .shifting

    case "**", "**!":
      self = .exponentiation

    default:
      return nil
    }
  }

  /// The weight of this precedence group. Groups with greater weights have higher precedence.
  public var weight: Int {
    switch self {
    case .assignment        : return  100
    case .logicalDisjunction: return  200
    case .logicalConjunction: return  300
    case .identifier        : return  400
    case .comparison        : return  500
    case .nilCoalescing     : return  600
    case .casting           : return  700
    case .addition          : return  800
    case .multiplication    : return  900
    case .shifting          : return 1000
    case .exponentiation    : return 1100
    }
  }

  /// The associativity of the operators in this group.
  public var associativity: Associativity {
    switch self {
    case .assignment, .exponentiation:
      return .right
    default:
      return .left
    }
  }

}
