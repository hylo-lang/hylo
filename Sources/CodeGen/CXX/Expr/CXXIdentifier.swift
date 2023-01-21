/// A string suitable as a C++ identifier.
public struct CXXIdentifier: CXXNode, CustomStringConvertible {

  /// The value of the identifier.
  public let description: String

  public init(_ identifier: String) {
    description = CXXIdentifier.sanitize(identifier)
  }

  /// Sanitizes `identifier` and returns a valid C++ identifier.
  public static func sanitize(_ identifier: String) -> String {
    // Append an underscore to reserved identifiers.
    reserved.contains(identifier)
      ? identifier + "_"
      : identifier
  }

  /// The set of reserved keywords in C++.
  public static let reserved = Set([
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

  public func writeCode<Target: TextOutputStream>(into target: inout Target) {
    target.write(description)
  }

}
