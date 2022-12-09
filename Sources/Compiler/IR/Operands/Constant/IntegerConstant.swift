import Utils

import BigInt

/// An unsigned integer Val IR constant.
public struct IntegerConstant: ConstantProtocol, Hashable {

  public let value: WideUInt // BigUInt//UInt64 //

  public init(_ value: BigUInt, bitWidth: Int) {
    self.value = WideUInt(truncatingIfNeeded: value, toWidth: bitWidth)
  }

  /// Creates a new integer Val IR constant with the specified bit pattern.
  public init<V: BinaryInteger>(_ value: V, bitWidth: Int) {
    self.value = WideUInt(truncatingIfNeeded: value, toWidth: bitWidth)
    // self.value = wide(exactly: value)!
//    self.value = WideUInt(truncatingIfNeeded: value, toWidth: bitWidth)
//    let ok = self.value == value
//    precondition(ok, "\(value) is not representable as UInt\(bitWidth)")
  }

  public var type: LoweredType { .object(BuiltinType.i(value.bitWidth)) }

}

extension IntegerConstant: CustomStringConvertible {

  public var description: String {
    "\(type.astType)(0x\(String(value.value, radix: 16)))"
  }

}
