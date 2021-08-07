import Basic

/// A token from a source file.
public struct Token {

  /// The kind of a token.
  public enum Kind: Int, CustomStringConvertible {

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
    case `continue`
    case `del`
    case `else`
    case `extn`
    case `for`
    case `fun`
    case `if`
    case `in`
    case `infix`
    case `match`
    case `mod`
    case `moveonly`
    case `mut`
    case `new`
    case `nil`
    case `postfix`
    case `prefix`
    case `pub`
    case `ret`
    case `static`
    case `type`
    case `val`
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

    public var description: String {
      switch self {
      case .bool      : return "Boolean literal"
      case .int       : return "integer literal"
      case .float     : return "floating point literal"
      case .string    : return "string literal"

      case .name      : return "name"
      case .under     : return "'_'"

      case .async     : return "'async'"
      case .await     : return "'await'"
      case .break     : return "'break'"
      case .case      : return "'case'"
      case .continue  : return "'continue'"
      case .del       : return "'del'"
      case .else      : return "'else'"
      case .extn      : return "'extn'"
      case .for       : return "'for'"
      case .fun       : return "'fun'"
      case .if        : return "'if'"
      case .in        : return "'in'"
      case .infix     : return "'infix'"
      case .match     : return "'match'"
      case .mod       : return "'mod'"
      case .moveonly  : return "'moveonly'"
      case .mut       : return "'mut'"
      case .new       : return "''new'"
      case .nil       : return "''nil'"
      case .postfix   : return "'postfix'"
      case .prefix    : return "'prefix'"
      case .pub       : return "'pub'"
      case .ret       : return "'ret'"
      case .static    : return "'static'"
      case .type      : return "'type'"
      case .val       : return "'val'"
      case .var       : return "'var'"
      case .view      : return "'view'"
      case .volatile  : return "'volatile'"
      case .where     : return "'where'"
      case .while     : return "'while'"

      case .oper      : return "operator"
      case .cast      : return "operator"
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
    case .infix, .mod, .moveonly, .mut, .postfix, .prefix, .pub, .static, .volatile:
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
    case .extn, .del, .fun, .new, .type, .val , .var, .view:
      return true
    default:
      return isDeclModifier
    }
  }

  /// A Boolean value indicating whether this token may begin a control statement.
  public var mayBeginCtrlStmt: Bool {
    switch kind {
    case .break, .continue, .for, .lBrace, .ret, .while:
      return true
    default:
      return false
    }
  }

}
