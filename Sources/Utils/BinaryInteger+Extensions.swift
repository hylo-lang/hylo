extension BinaryInteger {

  /// Returns `self` rounded up to the nearest multiple of `n`.
  ///
  /// - Precondition: `n` is nonzero and |`n`| is representable as `Self`.
  public func rounded(upToNearestMultipleOf n: Self) -> Self {
    precondition(n != 0)
    let m = Self(n.magnitude)
    let r = self % m
    return r == 0 ? self
      : self < 0 ? self - r
      : self + (m - r)
  }

}
