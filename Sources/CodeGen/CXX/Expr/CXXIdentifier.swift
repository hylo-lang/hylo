/// A string suitable as a C++ identifier.
struct CXXIdentifier: CXXExpr {

  /// The value of the identifier.
  let description: String

  /// Creates an instance sanitizing `n` for use as a C++ identifier.
  ///
  /// - Requires: `n` is not empty.
  init<S: StringProtocol>(_ n: S) {
    description = CXXIdentifier.sanitize(n)
  }

  var precedence: Int { 0 }

  var isLeftToRight: Bool {
    true  // doesn't really matter
  }

  /// Returns a copy of `n` sanitized for use as a C++ identifier.
  ///
  /// - Requires: `n` is not empty.
  static func sanitize<S: StringProtocol>(_ n: S) -> String {
    let s = n.reduce(into: "") { (s, c) in
      if c.isAllowedInCXXIdentitifer {
        s.append(c)
      } else {
        s.append(c.utf16.reduce(into: "u") { (u, point) in u += String(point, radix: 16) })
      }
    }

    if s.first!.isNumber {
      return "a" + s
    } else if reserved.contains(s) {
      return "_" + s
    } else {
      return s
    }
  }

  /// The set of reserved keywords in C++.
  static let reserved = Set([
    "asm", "auto", "break", "case",
    "catch", "char", "class", "const",
    "continue", "default", "delete", "do",
    "double", "else", "enum", "extern",
    "float", "for", "friend", "goto",
    "if", "inline", "int", "long",
    "signed", "sizeof", "static", "struct",
    "switch", "template", "this", "throw",
    "try", "typedef", "union", "unsigned",
    "virtual", "void", "volatile", "while",
  ])

}

extension CXXIdentifier: CustomStringConvertible {}

extension String.StringInterpolation {

  /// Appends a copy of `n` sanitized for use as a C++ identifier.
  ///
  /// - Requires: `n` is not empty.
  public mutating func appendInterpolation<S: StringProtocol>(cxx n: S) {
    appendLiteral(CXXIdentifier(n).description)
  }

}

extension Character {

  /// Indicates whether the character is allowed to appear in a C/C++ identifier.
  fileprivate var isAllowedInCXXIdentitifer: Bool {
    guard let code = asciiValue else { return false }
    return (0x5f == code)  // underscore
      || (0x61 ... 0x7a).contains(code)  // a ... z
      || (0x41 ... 0x5a).contains(code)  // A ... Z
      || (0x30 ... 0x39).contains(code)  // 0 ... 9
  }

}
