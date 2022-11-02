import Utils

/// An integer Val IR constant.
public struct IntegerConstant: ConstantProtocol, Hashable {

  /// The bit pattern of the integer.
  public let bitPattern: BitPattern

  /// Creates a new integer Val IR constant with the specified bit pattern.
  public init(bitPattern: BitPattern) {
    self.bitPattern = bitPattern
  }

  public var type: LoweredType { .object(.builtin(.i(bitPattern.width))) }

}

extension IntegerConstant: CustomStringConvertible {

  public var description: String {
    "\(type.astType)(0x\(bitPattern.hexadecimalString()))"
  }

}
