extension BinaryInteger {

  /// The factorial of `self`.
  ///
  /// - Requires: `self` is greater than or equal to zero.
  public var factorial: Self {
    func f(_ n: Self, _ a: Self) -> Self {
      n <= 0 ? a : f(n - 1, n * a)
    }
    return f(self, 1)
  }

}
