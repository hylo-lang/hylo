/// A digit in base 64.
///
/// The textual description of a `Base64Digit` is a single printable ASCII character, determined
/// with the following table:
///
///     ┌────────────────────────────────────────────────────────┐
///     | 00 ... 09 | \u{0030} ... \u{0039} | decimal digits     |
///     | 10 ... 35 | \u{0061} ... \u{007a} | lower case letters |
///     | 36 ... 61 | \u{0041} ... \u{005a} | upper case letters |
///     | 62        | \u{002e}              | period             |
///     | 63        | \u{005f}              | underscore         |
///     └────────────────────────────────────────────────────────┘
///
/// This character set is suitable for use in LLVM assembly identifiers.
public struct Base64Digit: Hashable {

  /// The value of the digit, in the range `0 ..< 64`.
  public let rawValue: UInt8

  /// Creates an instance from its raw value `n` or returns `nil` if `n >= 64`.
  public init?<T: BinaryInteger>(_ n: T) {
    if (n >= 0) && (n < 64) {
      self.rawValue = UInt8(n)
    } else {
      return nil
    }
  }

}

extension Base64Digit: RawRepresentable {

  /// Creates an instance from its raw value `n` or returns `nil` if `n >= 64`.
  public init?(rawValue n: UInt8) {
    self.init(n)
  }

}

extension Base64Digit: Comparable {

  /// Returns `true` iff `l` is less than `r`.
  public static func < (l: Self, r: Self) -> Bool {
    l.rawValue < r.rawValue
  }

}

extension Base64Digit: LosslessStringConvertible {

  /// Creates an instance from its textual representation.
  public init?(_ description: String) {
    guard description.count == 1 else {
      return nil
    }
    self.init(description[description.startIndex])
  }

  /// Creates an instance from its textual representation.
  public init?(_ description: Character) {
    guard let ascii = description.asciiValue else {
      return nil
    }
    self.init(ascii: ascii)
  }

  /// Creates an instance from its Unicode scalar representation.
  public init?(scalar: Unicode.Scalar) {
    guard scalar.value < 128 else {
      return nil
    }
    self.init(ascii: UInt8(scalar.value))
  }

  /// Creates an instance from its ASCII representation.
  public init?(ascii: UInt8) {
    if ascii == 46 {
      self.rawValue = 62
    } else if ascii == 95 {
      self.rawValue = 63
    } else if (ascii >= 48) && (ascii < 58) {
      self.rawValue = ascii - 48
    } else if (ascii >= 65) && (ascii < 91) {
      self.rawValue = ascii - (65 - 36)
    } else if (ascii >= 97) && (ascii < 123) {
      self.rawValue = ascii - (97 - 10)
    } else {
      return nil
    }
  }

  public var description: String {
    if rawValue < 10 {
      return String(Unicode.Scalar(rawValue + 48))
    } else if rawValue < 36 {
      return String(Unicode.Scalar(rawValue + (97 - 10)))
    } else if rawValue < 62 {
      return String(Unicode.Scalar(rawValue + (65 - 36)))
    } else if rawValue == 62 {
      return "."
    } else {
      return "_"
    }
  }

}
