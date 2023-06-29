import BigInt

/// An unisgned integer with an arbitrary, but fixed, bit width.
public struct WideUInt {

  /// The underlying value.
  ///
  /// Try to avoid using this; it's only public as part of our workarounds for
  /// https://github.com/apple/swift/issues/62498.
  public private(set) var value: BigUInt

  /// The fixed bit width; the maximum representable number is 2 to the `bitWidth` power minus one.
  public let bitWidth: Int

  /// Creates an instance with a bitWidth of `bitWidth`, equal to the low `bitWidth` bits of `bits`.
  public init<Bits: BinaryInteger>(truncatingIfNeeded bits: Bits, toWidth bitWidth: Int) {
    self = WideUInt(truncatingIfNeeded: BigUInt(bits), toWidth: bitWidth)
  }

  /// Creates an instance with a bitWidth of `bitWidth`, equal to the low `bitWidth` bits of `bits`.
  public init(truncatingIfNeeded bits: BigUInt, toWidth bitWidth: Int) {
    value = bits.truncated(toWidth: bitWidth)
    self.bitWidth = bitWidth
  }

}

extension WideUInt: Hashable {}

extension WideUInt: Comparable {

  public static func < (l: Self, r: Self) -> Bool { return l.value < r.value }

}

// DWA: Awaiting UnsignedInteger conformance pending https://github.com/apple/swift/issues/62498
extension WideUInt: Numeric, AdditiveArithmetic {

  public typealias Magnitude = Self

  public var magnitude: Magnitude { self }

  public static func <<= <RHS>(lhs: inout WideUInt, rhs: RHS) where RHS: BinaryInteger {
    lhs.value <<= rhs
    lhs.value.truncate(toWidth: lhs.bitWidth)
  }

  public static func >>= <RHS>(lhs: inout WideUInt, rhs: RHS) where RHS: BinaryInteger {
    lhs.value >>= rhs
    lhs.value.truncate(toWidth: lhs.bitWidth)  // rhs might be negative
  }

  public static func *= (lhs: inout WideUInt, rhs: WideUInt) {
    lhs = lhs.matchingWidth(rhs) { l, r in l * r }
  }

  public static func /= (lhs: inout WideUInt, rhs: WideUInt) {
    lhs = lhs.matchingWidth(rhs) { l, r in l / r }
  }

  public prefix static func ~ (x: Self) -> Self {
    WideUInt(truncatingIfNeeded: ~x.value, toWidth: x.bitWidth)
  }

  public init<T>(_ source: T) where T: BinaryInteger {
    value = BigUInt(source)
    bitWidth = source.bitWidth
  }

  public var words: BigUInt.Words {
    value.words
  }

  public init<T>(clamping source: T) where T: BinaryInteger {
    value = .init(clamping: source)
    bitWidth = source.bitWidth
  }

  public init<T>(truncatingIfNeeded source: T) where T: BinaryInteger {
    value = .init(truncatingIfNeeded: source)
    bitWidth = source.bitWidth
  }

  public init?<T>(exactly source: T) where T: BinaryInteger {
    value = BigUInt(exactly: source)!
    bitWidth = source.bitWidth
  }

  public init?<T>(exactly source: T, bitWidth: Int) where T: BinaryInteger {
    self = Self(truncatingIfNeeded: source, toWidth: bitWidth)
    if self.value != source { return nil }
  }

  public static func - (lhs: WideUInt, rhs: WideUInt) -> WideUInt {
    lhs.matchingWidth(rhs) { l, r in l - r }
  }

  public init(integerLiteral literal: BigUInt.IntegerLiteralType) {
    self.value = .init(integerLiteral: literal)
    self.bitWidth = value.bitWidth
  }

  public typealias Words = BigUInt.Words

  public init?<T>(exactly source: T) where T: BinaryFloatingPoint {
    guard let value = BigUInt(exactly: source) else { return nil }
    self.value = value
    bitWidth = Int.max
  }

  public init<T>(_ source: T) where T: BinaryFloatingPoint {
    value = .init(source)
    bitWidth = Int.max
  }

  public var trailingZeroBitCount: Int { value.trailingZeroBitCount }

  public static func / (lhs: WideUInt, rhs: WideUInt) -> WideUInt {
    lhs.matchingWidth(rhs) { l, r in l / r }
  }

  public static func % (lhs: WideUInt, rhs: WideUInt) -> WideUInt {
    lhs.matchingWidth(rhs) { l, r in l % r }
  }

  public static func %= (lhs: inout WideUInt, rhs: WideUInt) {
    lhs.value %= lhs.matchingWidth(rhs) { (l, r) in r }.value
  }

  public static func * (lhs: WideUInt, rhs: WideUInt) -> WideUInt {
    lhs.matchingWidth(rhs) { l, r in l * r }
  }

  public static func &= (lhs: inout WideUInt, rhs: WideUInt) {
    lhs.value &= lhs.matchingWidth(rhs) { l, r in r }.value
  }

  public static func |= (lhs: inout WideUInt, rhs: WideUInt) {
    lhs.value |= lhs.matchingWidth(rhs) { l, r in r }.value
  }

  public static func ^= (lhs: inout WideUInt, rhs: WideUInt) {
    lhs.value ^= lhs.matchingWidth(rhs) { l, r in r }.value
  }

  public static func + (lhs: WideUInt, rhs: WideUInt) -> WideUInt {
    lhs.matchingWidth(rhs) { l, r in l + r }
  }

  public typealias IntegerLiteralType = BigUInt.IntegerLiteralType

  /// Returns an instance with the same `bitWidth` as `self` whose value is the (truncated if
  /// necessary) result of `combine(self.value, rhs.value)`.
  ///
  /// - Precondition: `self.bitWidth` == `rhs.bitWidth`
  private func matchingWidth(_ rhs: WideUInt, combine: (BigUInt, BigUInt) -> BigUInt) -> WideUInt {
    precondition(
      bitWidth == rhs.bitWidth,
      "mixed-width operations not allowed (\(bitWidth) vs \(rhs.bitWidth)")
    return WideUInt(truncatingIfNeeded: combine(value, rhs.value), toWidth: bitWidth)
  }

}

extension WideUInt: CustomStringConvertible {

  public var description: String { String(describing: value) }

}

extension BigUInt {

  /// Discards all but the low `w` bits of `self`.
  fileprivate mutating func truncate(toWidth w: Int) {
    self &= ((1 as Self) << w) - 1
  }

  /// Returns `self`, truncated to fit in `w` bits.
  fileprivate func truncated(toWidth w: Int) -> Self {
    self & (((1 as Self) << w) - 1)
  }

}
