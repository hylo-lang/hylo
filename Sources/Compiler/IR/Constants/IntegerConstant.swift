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

extension IntegerConstant: CustomStringConvertible {

  public var description: String {
    var hex: [UInt8] = []
    var sum: UInt8 = 0
    var digit = 0

    for bit in bitPattern {
      switch digit {
      case 0:
        sum = bit ? 1 : 0
        digit = 1
      case 1:
        if bit { sum += 2 }
        digit = 2
      case 2:
        if bit { sum += 4 }
        digit = 3
      default:
        if bit { sum += 8 }
        digit = 0
        hex.append(sum)
      }
    }

    if digit != 0 { hex.append(sum) }

    return hex
      .dropLast(while: { $0 == 0 })
      .reversed()
      .reduce(into: "\(type.valType) 0x", { (result, d) in
        result.append(d < 10
          ? Character(UnicodeScalar(48 + d))
          : Character(UnicodeScalar(87 + d)))
      })
  }

}
