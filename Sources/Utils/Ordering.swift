/// The result type of a three-way comparison implementing a strict total order.
public enum StrictOrdering: Hashable {

  /// The LHS is ordered before the RHS.
  case ascending

  /// The LHS is neither ordered before nor ordered after the RHS.
  case equal

  /// The LHS is ordered after the RHS.
  case descending

  /// Creates the comparison of `a` with `b`.
  public init<T: Comparable>(between a: T, and b: T) {
    self = (a < b) ? .ascending : ((b < a) ? .descending : .equal)
  }

}

/// The result type of a three-way comparison implementing a strict partial order.
public typealias StrictPartialOrdering = StrictOrdering?
