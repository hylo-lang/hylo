/// Computes the next aligned offset from a base address assumed to be aligned.
///
/// - Parameters:
///   - alignment: The designed alignment.
///   - offset: An offset from a base address, possibly misaligned.
func nextOffset(alignedAt alignment: Int, from offset: Int) -> Int {
  // An offset can be rounded up to the required alignment by adding the alignment decremented by
  // one and mask the result by the negation of that decrement.
  return (offset + alignment - 1) & ~(alignment - 1)
}

/// Returns `true` if the given type is trivial and can can be byte-wise copied.
func isTrivial<T>(_ type: T.Type) -> Bool {
  _isPOD(type)
}

extension Int {

  /// Returns the closest multiple of `value` whose magnitude is greater than or equal to that of
  /// this integer.
  func roundedAwayFromZero(toNearestMultipleOf value: Int) -> Int {
    return self % value == 0
      ? self
      : self + (value - self % value)
  }

  /// Returns this value rounded up to the next power of 2.
  var nextPowerOfTwo: Int {
    var v = self - 1
    v |= v >> 1
    v |= v >> 2
    v |= v >> 4
    v |= v >> 8
    v |= v >> 16
#if arch(x86_64) || arch(arm64) || arch(s390x) || arch(powerpc64) || arch(powerpc64le)
    v |= v >> 32
#endif
    return v + 1
  }

}
