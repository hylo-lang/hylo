extension Character {

  /// A Boolean value indicating whether this character represents a decimal digit.
  var isDigit: Bool {
    return (0x30 ... 0x39) ~= (asciiValue ?? 0)
  }

  /// A Boolean value indicating whether this character represents an hexadecimal digit.
  var isHexDigit: Bool {
    guard let ascii = asciiValue else { return false }
    return (0x30 ... 0x39) ~= ascii // 0 ... 9
        || (0x41 ... 0x46) ~= ascii // A ... F
        || (0x61 ... 0x66) ~= ascii // a ... f
  }

  /// A Boolean value indicating whether this character represents an octal digit.
  var isOctDigit: Bool {
    return (0x30 ... 0x37) ~= (asciiValue ?? 0)
  }

  /// A Boolean value indicating whether this character represents a binary digit.
  var isBinDigit: Bool {
    return self == "0" || self == "1"
  }

  /// A Boolean value indicating whether this character represents an operator.
  var isOperator: Bool {
    return "<>=+-*/%&|!?^~".contains(self)
  }

}
