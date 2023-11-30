import Core
import Utils

// DWA: This conformance belongs in WideUInt.swift, but is here pending
// https://github.com/apple/swift/issues/62498.
#if swift(>=5.10)
extension WideUInt: @retroactive UnsignedInteger {}
#else
extension WideUInt: UnsignedInteger {}
#endif

/// An unsigned integer Hylo IR constant.
public struct IntegerConstant: Constant, Hashable {

  public let value: WideUInt

  /// Creates a new integer Hylo IR constant with value `x` and the given `bitWidth`.
  ///
  /// - Precondition: `x` is non-negative and representable in `bitWidth` bits.
  public init<V: BinaryInteger>(_ x: V, bitWidth: Int) {
    self.value = WideUInt(exactly: x, bitWidth: bitWidth)!
  }

  /// Creates a new Hylo IR constant with given `value`.
  public init(_ value: WideUInt) {
    self.value = value
  }

  public var type: IR.`Type` { .object(BuiltinType.i(value.bitWidth)) }

}

extension IntegerConstant: CustomStringConvertible {

  public var description: String {
    "\(type.ast)(0x\(String(value.value, radix: 16)))"
  }

}
