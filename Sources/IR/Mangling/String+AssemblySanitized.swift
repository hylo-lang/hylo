extension String {

  /// Given a string returned by `assemblySanitized`, creates an instance decoding its contents.
  public init?(assemblySanitized sanitized: String) {
    guard var i = sanitized.lastIndex(of: "$") else {
      self = sanitized
      return
    }

    var decoded = Array(sanitized[..<i].unicodeScalars)
    var offset = 0

    i = sanitized.index(after: i)
    while i != sanitized.endIndex {
      guard
        let (delta, j) = Base64VarUInt.decode(from: sanitized[i...]),
        let (point, k) = Base64VarUInt.decode(from: sanitized[j...])
      else { return nil }

      offset += Int(delta.rawValue)
      decoded.insert(Unicode.Scalar(UInt32(point.rawValue))!, at: offset)
      i = k
    }

    self = decoded.reduce(into: "", { (s, p) in s.append(contentsOf: String(p)) })
  }

  /// `true` iff `self` is suitable as an identifier in LLVM assembly.
  ///
  /// - Complexity: O(*n*) where *n* is the length of `self`.
  public var isAssemblySuitable: Bool {
    unicodeScalars.allSatisfy { (point) in
      (point == "$") || (Base64Digit(scalar: point) != nil)
    }
  }

  /// Returns an encoding of `self` sanitized for use as an identifier in LLVM assembly.
  ///
  /// `self` is returned unchanged if and only if it does not contain any dollar sign ("$") and
  /// `self.isAssemblySuitable` is `true`. Otherwise, the returned value is the concatenation of
  /// two strings of `Base64Digit` separated by a unique occurrence of the dollar sign. The part
  /// on the LHS is called the *payload*, the other is called the *instruction sequence*.
  ///
  /// The payload contains a copy of the characters in `self` that are allowed in LLVM assembly
  /// identifiers, in the same order. The instruction sequence is a list of pairs `(d, p)` where
  /// `p` is a code point and `d` denotes the position at which `p` should be inserted to decode
  /// the string. Both values are encoded as instances of `Base64VarUInt`.
  ///
  /// The decoding of a sanitized payload can be described as follows:
  ///
  ///     var offset = 0
  ///     for (d, p) in instructions {
  ///       offset += d
  ///       payload.insert(p, at: d)
  ///     }
  ///
  /// For example, let the input be the mangled string "infix$5P91Pa". The payload is "infix" and
  /// the instruction sequence is composed of the pairs ("5", "P9") and ("1", "Pa"). The first
  /// indicates that the unicode point 60 must be inserted in the payload at offset 5. The second
  /// indicates that the unicode point 61 must be inserted in the payload at offset 5 + 1.
  public var assemblySanitized: String {
    if unicodeScalars.allSatisfy({ Base64Digit(scalar: $0) != nil }) {
      return self
    }

    var result = ""
    var instructions = ""
    var previousInsertionOffset = 0

    for (offset, point) in unicodeScalars.enumerated() {
      if Base64Digit(scalar: point) != nil {
        result.append(Character(point))
        continue
      }

      let delta = offset - previousInsertionOffset
      Base64VarUInt(delta).write(to: &instructions)
      Base64VarUInt(point.value).write(to: &instructions)
      previousInsertionOffset = offset
    }

    return result + "$" + instructions
  }

}
