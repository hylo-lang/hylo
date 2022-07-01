/// An operator precedence group.
public enum PrecedenceGroup: String, Hashable {

  /// An operator associativity.
  public enum Associativity: Hashable {

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

  /// The binding power of the operators in the group.
  public var power: Int {
    switch self {
    case .assignment      : return 1
    case .disjunction     : return 2
    case .conjunction     : return 3
    case .comparison      : return 4
    case .fallback        : return 5
    case .range           : return 6
    case .addition        : return 7
    case .multiplication  : return 8
    case .shift           : return 9
    }
  }

  /// The associativity of the operators in the group, if any.
  public var associativity: Associativity? {
    switch self {
    case .assignment, .fallback:
      return .right
    case .disjunction, .conjunction, .comparison, .addition, .multiplication, .shift:
      return .left
    case .range:
      return nil
    }

  }

}

extension PrecedenceGroup: Comparable {

  public static func < (l: Self, r: Self) -> Bool {
    l.power < r.power
  }

}
