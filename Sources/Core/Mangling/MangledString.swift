/// A string of alphanumeric ASCII characters and underscores encoding an arbitrary Unicode string.
///
/// The mangled representation is a string that only contains alphanumeric ASCII characters and
/// underscores. It has the following grammar:
///
///     mangled-string ::=
///       '_'
///       non-zero-manged-string
///
///     non-zero-manged-string ::= (token)
///       mangled-string-length mangled-string-payload
///
///     mangled-string-length ::= (regex)
///       [a-zA-Z0-9_]*
///
///     mangled-string-payload ::= (regex)
///       [a-zA-Z0-9_]*
///
/// A single underscore denotes the empty string. In non-empty strings, `<mangled-string-length>`
/// encodes the number of characters in `<mangled-string-payload>` in base 62 (lower-case letters
/// are lesser than upper-case ones). A leading "0" indicates that the payload is *sanitized* and
/// and must be decoding. Otherwise, it is a perfect copy of the original string. (Note: as empty
/// strings have a special representation, the next character after a leading "0" cannot be part
/// of the payload).
///
/// The first non-zero character of a length is the number of digits in its representation. The
/// remaining characters are digits of a number in base 62 that reads from right to left, that is,
/// the most significant digit is rightmost). For example, "231" represents the number 65 (two
/// digits; 3 x 62^0 + 1 x 62^1).
///
/// A sanitized payload is composed of a *literal* and an *instruction sequence*, separated by an
/// underscore. The instruction sequence only contains alphanumeric ASCII characters, meaning that
/// the last underscore in the payload marks the separation. The literal contains a copy of the
/// alphanumeric ASCII characters and underscores in the input string, in the same order. The
/// instruction sequence is a list of pairs `(d, p)` where `p` is a code point and `d` denotes the
/// position at which `p` should be inserted in the decoded payload string.
///
/// `d` is encoded as a sequence of digits in base 62 that have to be summed. The end of the
/// sequence is signaled by the first non-"Z" digit. For example, "Za" represents the number 71
/// (61 + 10) and "ZZ0" the number 122 (61 + 61 + 0). `d` is always in canonical form, defined as
/// a sequence of exactly one non-"Z" digit prefixed by any number of "Z"-digits. `p` is encoded
/// the same way as lengths.
///
/// The decoding of a sanitized payload can be described as follows:
///
///     var offset = 0
///     for (d, p) in instructions {
///       offset += d
///       literal.insert(p, at: d)
///     }
///
/// For example, let the input be the mangled string "01cinfix_51Y11Z". "01c" encodes the number
/// 12, which is the length of a sanitized payload. The literal is "infix" and the instruction
/// sequence is composed of the pairs ("5", "1Y") and ("1", "1Z"). The first indicates that the
/// unicode point 60 must be inserted in the literal at offset 5. The second indicates that the
/// unicode point 61 must be inserted in the literal at offset 5 + 1.
public struct MangledString: Hashable, Codable {

  /// The raw value of this instance.
  public let rawValue: String

  /// Creates an instance mangling `s`.
  public init(_ s: String) {
    self.rawValue = Self.mangle(string: s)
  }

}

extension MangledString: Comparable {

  /// Returns `true` iff `l` lexicographically precedes `r`.
  public static func < (l: Self, r: Self) -> Bool {
    l.rawValue.lexicographicallyPrecedes(r.rawValue)
  }

}

extension MangledString: CustomStringConvertible {

  /// The demangled representation of `self`.
  public var description: String {
    Self.demangle(string: rawValue)
  }

}

extension MangledString {

  /// Returns the mangled representation of `input`.
  private static func mangle(string input: String) -> String {
    // Empty string is mangled as "_".
    if input.isEmpty {
      return "_"
    }

    // Inputs that only contains alphanumeric ASCII + "_" don't require character encoding.
    if input.unicodeScalars.allSatisfy(\.isMangleAllowed) {
      return encode(number: UInt32(input.count)) + input
    }

    var result = ""
    var instructions = ""
    var previousInsertionOffset = 0

    for (offset, point) in input.unicodeScalars.enumerated() {
      if point.isMangleAllowed {
        result.append(Character(point))
        continue
      }

      instructions.append(encode(offset: UInt32(offset - previousInsertionOffset)))
      instructions.append(encode(number: point.value))
      previousInsertionOffset = offset
    }

    result.append("_")
    result.append(instructions)
    return "0" + encode(number: UInt32(result.count)) + result
  }

  /// Returns a string representation of `n` for use as an offset in the decoding instructions.
  private static func encode(offset n: UInt32) -> String {
    if n < 61 {
      return encode(digit: n)
    } else {
      return encode(digit: 61) + encode(offset: n - 61)
    }
  }

  /// Returns a string representation of `n` for use as a number in a mangled string.
  private static func encode(number: UInt32) -> String {
    var result = ""

    if number == 0 {
      return "0"
    }

    var v = number
    while v > 0 {
      result.append(encode(digit: v % 62))
      v /= 62
    }

    if result.count == 1 {
      return "1" + result
    } else {
      return encode(digit: UInt32(result.count)) + result
    }
  }

  /// Returns `digit` encoded as an ASCII digit in base 62.
  ///
  /// - Requires: `digit` is in the range `0 ..< 62`
  private static func encode(digit: UInt32) -> String {
    if digit < 10 {
      return String(Unicode.Scalar(digit + 48)!)
    } else if digit < 36 {
      return String(Unicode.Scalar(digit + (97 - 10))!)
    } else if digit < 62 {
      return String(Unicode.Scalar(digit + (65 - 36))!)
    } else {
      preconditionFailure("overflow")
    }
  }

  /// Returns the demangled representation of `input` or `nil` if it can't be parsed.
  ///
  /// - Requires: `input` only contains ASCII characters.
  private static func demangle(string input: String) -> String {
    var s = input[input.startIndex...]
    return parseMangledString(from: &s)
  }

  /// Parses a mangled string at the start of `stream` and returns its demangled representation,
  /// advancing `stream` to the first character after the parsed string.
  ///
  /// - Requires: `stream` only contains ASCII characters.
  public static func parseMangledString(from stream: inout Substring) -> String {
    // "_" is the empty string.
    if stream.starts(with: "_") {
      stream.removeFirst()
      return ""
    }

    let points = stream.unicodeScalars
    let (result, i) = parseMangledString(from: points)

    stream.removeFirst(points.distance(from: points.startIndex, to: i))
    return result
  }

  /// Parses a mangled string at the start of `stream` and returns its demangled representation
  /// along with the index of the first character after the parsed string.
  private static func parseMangledString<S: BidirectionalCollection<Unicode.Scalar>>(
    from stream: S
  ) -> (String, S.Index) {
    // A leading "0" means that the payload must be decoded.
    if stream.first == "0".unicodeScalars.first! {
      let (length, i) = parseNumber(from: stream[stream.index(after: stream.startIndex)...])
      let j = stream.index(i, offsetBy: Int(length))
      let s = decode(payload: stream[i ..< j])
      return (s, j)
    } else {
      let (length, i) = parseNumber(from: stream)
      let j = stream.index(i, offsetBy: Int(length))
      let s = stream[i ..< j].reduce(into: "", { (s, p) in s.append(contentsOf: String(p)) })
      return (s, j)
    }
  }

  /// Parses an offset at the start of `stream`, which contains decoding instructions, returning
  /// the parsed value along with the index of the first character that follows it.
  private static func parseOffset<S: BidirectionalCollection<Unicode.Scalar>>(
    from stream: S
  ) -> (UInt32, S.Index) {
    var result: UInt32 = 0
    var i = stream.startIndex

    while true {
      let n = decode(digit: stream[i])
      i = stream.index(after: i)

      result += n
      if n < 61 {
        return (result, i)
      }
    }
  }

  /// Parses a number at the start of `stream`, returning the parsed value along with the index of
  /// the first character that follows it.
  private static func parseNumber<S: BidirectionalCollection<Unicode.Scalar>>(
    from stream: S
  ) -> (UInt32, S.Index) {
    var i = stream.startIndex

    // The first code point encodes the number of digits in the number.
    let width = decode(digit: stream[i])
    if width == 0 {
      return (0, stream.index(after: i))
    }

    var result: UInt32 = 0
    var weight: UInt32 = 1
    for _ in 0 ..< width {
      i = stream.index(after: i)
      result += decode(digit: stream[i]) * weight
      weight *= 62
    }
    return (result, stream.index(after: i))
  }

  /// Returns the decoded value of the `input`, which is a sanitized payload.
  private static func decode<S: BidirectionalCollection<Unicode.Scalar>>(
    payload input: S
  ) -> String {
    let separator = input.lastIndex(of: "_".unicodeScalars.first!)!

    // Code points before the separator is copied as-is. Code points after the separator represent
    // the decoding instructions.
    var result = Array(input[input.startIndex ..< separator])
    var instructions = input[input.index(after: separator)...]
    var insertionOffset = 0

    while !instructions.isEmpty {
      // Compute the offset of the next character to insert.
      let (offset, i) = parseOffset(from: instructions)
      let (number, j) = parseNumber(from: instructions[i...])
      insertionOffset += Int(offset)

      // Insert the decoded character.
      result.insert(Unicode.Scalar(number)!, at: insertionOffset)
      instructions = instructions[j...]
    }

    return result.reduce(into: "", { (s, p) in s.append(contentsOf: String(p)) })
  }

  /// Returns the decoded value of `digit`, which is an ASCII digit in base 62.
  private static func decode(digit: Unicode.Scalar) -> UInt32 {
    if digit.value < 58 {
      return digit.value - 48
    } else if digit.value < 91 {
      return digit.value - (65 - 36)
    } else if digit.value < 123 {
      return digit.value - (97 - 10)
    } else {
      preconditionFailure("overflow")
    }
  }

}

extension Unicode.Scalar {

  /// `true` iff `self` is an ASCII character allowed in a mangled symbol.
  fileprivate var isMangleAllowed: Bool {
    return (0x5f == value)  // underscore
      || (0x61 ... 0x7a).contains(value)  // a ... z
      || (0x41 ... 0x5a).contains(value)  // A ... Z
      || (0x30 ... 0x39).contains(value)  // 0 ... 9
  }

}
