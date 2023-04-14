import Core
import Utils

// DWA: This conformance belongs in WideUInt.swift, but is here pending
// https://github.com/apple/swift/issues/62498.
extension WideUInt: UnsignedInteger {}

/// An unsigned integer Val IR constant.
public struct IntegerConstant: ConstantProtocol, Hashable {

  public let value: WideUInt

  /// Creates a new integer Val IR constant with value `x` and the given `bitWidth`.
  ///
  /// - Precondition: `x` is non-negative and representable in `bitWidth` bits.
  public init<V: BinaryInteger>(_ x: V, bitWidth: Int) {
    self.value = WideUInt(exactly: x, bitWidth: bitWidth)!
  }

  /// Creates a new Val IR constant with given `value`.
  public init(_ value: WideUInt) {
    self.value = value
  }

  public var type: LoweredType { .object(BuiltinType.i(value.bitWidth)) }

}

extension IntegerConstant: CustomStringConvertible {

  public var description: String {
    "\(type.ast)(0x\(String(value.value, radix: 16)))"
  }

}
