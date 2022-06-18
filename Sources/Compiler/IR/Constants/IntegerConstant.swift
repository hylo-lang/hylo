import Utils

/// An integer VIR constant.
public struct IntegerConstant: ConstantProtocol {

  /// The bit pattern of the integer.
  public var bitPattern: BitPattern

  /// Creates a new integer VIR constant with the specified bit pattern.
  public init(bitPattern: BitPattern) {
    self.bitPattern = bitPattern
  }

  public var type: IRType { .owned(.builtin(.i(bitPattern.width))) }

}
