/// A Boolean extended with an indeterminate third value.
public typealias ThreeValuedBit = Bool?

public extension ThreeValuedBit {

  /// Returns `false` if `b` is `true`, `true` if `b` is `false`, or `nil` otherwise.
  static prefix func ! (b: Self) -> Self {
    b.map(!)
  }

  /// Returns `false` if either of the operands is `false`, `true` if both operands are `true`,
  /// `nil` in all other cases.
  static func && (l: Self, r: Self) -> Self {
    if let a = l {
      return a ? r : false
    }
    if let b = r {
      return b ? l : false
    }
    return nil
  }

  /// Returns `false` if both operands are `false`, `true` if either of the operands is `true`,
  /// `nil` in all other cases.
  static func || (l: Self, r: Self) -> Self {
    if let a = l {
      return a ? true : r
    }
    if let b = r {
      return b ? true : l
    }
    return nil
  }

}
