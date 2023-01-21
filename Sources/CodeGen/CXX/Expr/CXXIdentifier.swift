/// A string suitable as a C++ identifier.
struct CXXIdentifier: CXXExpr {

  /// The value of the identifier.
  let description: String

  init(_ identifier: String) {
    description = CXXIdentifier.sanitize(identifier)
  }

  /// Sanitizes `identifier` and returns a valid C++ identifier.
  static func sanitize(_ identifier: String) -> String {
    // Append an underscore to reserved identifiers.
    reserved.contains(identifier)
      ? identifier + "_"
      : identifier
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
