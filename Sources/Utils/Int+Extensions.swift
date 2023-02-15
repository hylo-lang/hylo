extension Int {

  /// Returns `self` rounded up to the next power of two.
  ///
  /// - Requires: `self` must be a positive integer.
  public var roundedUpToNextPowerOfTwo: Int {
    var x = UInt(bitPattern: self &- 1)
    x |= x &>> 1
    x |= x &>> 2
    x |= x &>> 4
    x |= x &>> 8
    x |= x &>> 16
    #if (arch(x86_64) || arch(arm64))
      x |= x &>> 32
    #elseif (!arch(i386) && !arch(arm))
      if Int.bitWidth > 32 {
        x |= x &>> 32
      }
    #endif
    return Int(bitPattern: x &+ 1)
  }

}
