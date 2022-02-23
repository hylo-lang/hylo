/// A token from a source file.
public struct Token {

  /// The kind of a token.
  public enum Kind: Int {

    // Errors
    case invalid  = 0
    case unterminatedString
    case unterminatedBlockComment

    // Scalar literals
    case bool     = 1000
    case int
    case float
    case string

    // Identifiers
    case name     = 2000
    case under

    // Keywords
    case `async`  = 3000
    case `await`
    case `break`
    case `case`
    case `consuming`
    case `continue`
    case `del`
    case `else`
    case `extension`
    case `for`
    case `fun`
    case `if`
    case `in`
    case `infix`
    case `let`
    case `local`
    case `match`
    case `mod`
    case `mut`
    case `namespace`
    case `new`
    case `nil`
    case `postfix`
    case `prefix`
    case `pub`
    case `return`
    case `static`
    case `type`
    case `var`
    case `view`
    case `volatile`
    case `where`
    case `while`

    // Operators
    case oper     = 4000
    case cast
    case arrow
    case assign

    // Punctuation
    case comma    = 5000
    case semi
    case dot
    case colon
    case twoColons

    // Delimiters
    case lParen   = 6000
    case rParen
    case lBrace
    case rBrace
    case lBrack
    case rBrack
    case lAngle
    case rAngle

    public var longDescription: String {
      switch self {
      case .bool      : return "Boolean literal"
      case .int       : return "integer literal"
      case .float     : return "floating point literal"
      case .string    : return "string literal"

      case .name      : return "name"
      case .under     : return "'_'"

      case .oper      : return "operator"
      case .cast      : return "casting operator"
      case .arrow     : return "'->'"
      case .assign    : return "'='"

      case .comma     : return "','"
      case .semi      : return "';'"
      case .dot       : return "'.'"
      case .colon     : return "':'"
      case .twoColons : return "'::'"

      case .lParen    : return "'('"
      case .rParen    : return "')'"
      case .lBrace    : return "'{'"
      case .rBrace    : return "'}'"
      case .lBrack    : return "'['"
      case .rBrack    : return "']'"
      case .lAngle    : return "'<'"
      case .rAngle    : return "'>'"

      case .invalid: return "invalid token"
      case .unterminatedString: return "unterminated string"
      case .unterminatedBlockComment: return "unterminated block comment"

      default: return "'\(self)'"
      }
    }

  }

  /// The kind of the token.
  public internal(set) var kind: Kind

  /// The range of the token in the source file from which it has been parsed.
  public internal(set) var range: SourceRange

  /// A Boolean value indicating whether the token is a keyword.
  public var isKeyword: Bool {
    return (Kind.async.rawValue ..< Kind.async.rawValue + 999) ~= kind.rawValue
  }

  /// A Boolean value indicating whether the token may be used as a label.
  public var isLabel: Bool {
    return (kind == .name) || isKeyword
  }

  /// A Boolean value indicating whether the token may be used as an operator.
  ///
  /// Prefer using this property rather than checking `token.kind == .oper` to determine whether
  /// a token may represent an operator. The assignment operator (i.e., `=`) and the angle brackets
  /// (i.e., `<` and `>`) do not have the kind `.oper`.
  public var isOperator: Bool {
    return isOf(kind: [.oper, .assign, .lAngle, .rAngle])
  }

  /// A Boolean value indicating whether the token is a declaration modifier.
  public var isDeclModifier: Bool {
    switch kind {
    case .consuming, .infix, .mod, .mut, .postfix, .prefix, .pub, .static, .volatile:
      return true
    default:
      return false
    }
  }

  /// Returns whether the token's kind is one of the specified values.
  ///
  /// - Parameter kinds: A list of kinds.
  public func isOf(kind kinds: [Token.Kind]) -> Bool {
    return kinds.contains(kind)
  }

  /// A Boolean value indicating whether this token may begin a declaration.
  public var mayBeginDecl: Bool {
    switch kind {
    case .extension, .del, .fun, .let, .namespace, .new, .type, .var, .view:
      return true
    default:
      return isDeclModifier
    }
  }

  /// A Boolean value indicating whether this token may begin a control statement.
  public var mayBeginCtrlStmt: Bool {
    switch kind {
    case .break, .continue, .for, .if, .lBrace, .return, .while:
      return true
    default:
      return false
    }
  }

}
