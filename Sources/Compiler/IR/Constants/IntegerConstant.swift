import Utils

/// An integer Val IR constant.
public struct IntegerConstant: ConstantProtocol, Hashable {

  /// The bit pattern of the integer.
  public var bitPattern: BitPattern

  /// Creates a new integer Val IR constant with the specified bit pattern.
  public init(bitPattern: BitPattern) {
    self.bitPattern = bitPattern
  }

  public var type: IRType { .owned(.builtin(.i(bitPattern.width))) }

}
