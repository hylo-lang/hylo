extension Int {

  /// Returns `self` rounded up to the nearest multiple of `stride`.
  public func round(upToNearestMultipleOf stride: Int) -> Int {
    if stride == 0 {
      return self
    }

    let r = abs(self) % stride
    if r == 0 {
      return self
    } else if r < 0 {
      return -(abs(self) - r)
    }

    return self + stride - r
  }

}
