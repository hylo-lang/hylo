extension BinaryInteger {

  /// Returns `self` rounded up to the nearest multiple of `n`.
  ///
  /// - Precondition: `n` is nonzero
  /// - Precondition: `self` is negative, or `self` + |n| - 1 is representable as `Self`.
  public func rounded(upToNearestMultipleOf n: Self) -> Self {
    precondition(n != 0)
    let m = Self(n.magnitude)
    return (self < 0 ? self : self + (m - 1)) / m * m
  }

}
