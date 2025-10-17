/// An operator precedence group.
public enum PrecedenceGroup: String, Codable, Sendable {

  /// An operator associativity.
  public enum Associativity: Codable, Sendable {

    case left

    case right

  }

  case assignment

  case disjunction

  case conjunction

  case comparison

  case fallback

  case range

  case addition

  case multiplication

  case shift

  case exponentiation

  /// The binding power of the operators in the group.
  public var power: Int {
    switch self {
    case .assignment: return 1
    case .disjunction: return 2
    case .conjunction: return 3
    case .comparison: return 4
    case .fallback: return 5
    case .range: return 6
    case .addition: return 7
    case .multiplication: return 8
    case .shift: return 9
    case .exponentiation: return 10
    }
  }

  /// The associativity of the operators in the group.
  public var associativity: Associativity {
    switch self {
    case .assignment, .fallback, .exponentiation:
      return .right
    case .disjunction, .conjunction, .comparison, .addition, .multiplication, .range, .shift:
      return .left
    }

  }

}

extension PrecedenceGroup: Comparable {

  public static func < (l: Self, r: Self) -> Bool {
    l.power < r.power
  }

}
