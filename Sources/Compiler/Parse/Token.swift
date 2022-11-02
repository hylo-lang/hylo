/// A terminal symbol of the syntactic grammar.
public struct Token {

  /// The kind of a token.
  public enum Kind: Int {

    // Errors
    case invalid = 0
    case unterminatedString
    case unterminatedBlockComment

    // Scalar literals
    case bool = 1000
    case int
    case float
    case string

    // Identifiers
    case name = 2000
    case under

    // Keywords
    case `any` = 3000
    case `async`
    case `await`
    case `break`
    case `catch`
    case `conformance`
    case `continue`
    case `deinit`
    case `do`
    case `else`
    case `extension`
    case `for`
    case `fun`
    case `if`
    case `import`
    case `in`
    case `indirect`
    case `infix`
    case `init`
    case `inout`
    case `let`
    case `match`
    case `namespace`
    case `nil`
    case `operator`
    case `postfix`
    case `prefix`
    case `property`
    case `public`
    case `return`
    case `set`
    case `sink`
    case `some`
    case `static`
    case `subscript`
    case `trait`
    case `try`
    case `type`
    case `typealias`
    case `var`
    case `where`
    case `while`
    case `yield`
    case `yielded`

    // Attributes
    case attribute = 4000

    // Operators
    case oper = 5000
    case ampersand
    case cast
    case arrow
    case assign
    case equal
    case pipe

    // Punctuation
    case comma = 6000
    case semi
    case dot
    case colon
    case twoColons

    // Delimiters
    case lParen = 7000
    case rParen
    case lBrace
    case rBrace
    case lBrack
    case rBrack
    case lAngle
    case rAngle

  }

  /// The kind of the token.
  public internal(set) var kind: Kind

  /// The range of the token in the source file from which it has been parsed.
  public internal(set) var range: SourceRange

  /// Indicates whether `self` is a keyword.
  public var isKeyword: Bool {
    (Kind.async.rawValue ..< Kind.async.rawValue + 999) ~= kind.rawValue
  }

  /// Indicates whether `self` is a suitable as a label.
  public var isLabel: Bool {
    (kind == .name) || isKeyword
  }

  /// Indicates whether `self` may be in an operator
  ///
  /// Use this property rather than testing `self.kind == .oper` as some operators (e.g., `==`) do
  /// not have the kind `.oper`.
  public var isOperatorToken: Bool {
    isOf(kind: [.oper, .ampersand, .assign, .equal, .pipe, .lAngle, .rAngle])
  }

  /// Indicates whether `self` is a suitable prefix operator head.
  public var isPrefixOperatorHead: Bool {
    isOf(kind: [.oper, .rAngle, .equal, .pipe])
  }

  /// Indicates whether `self` is a suitable postfix operator head.
  public var isPostfixOperatorHead: Bool {
    isOf(kind: [.oper, .lAngle, .equal, .pipe, .ampersand])
  }

  /// Indicates whether `self` is a declaration modifier.
  public var isDeclModifier: Bool {
    switch kind {
    case .infix, .postfix, .prefix, .public, .static:
      return true
    default:
      return false
    }
  }

  /// Indicates whether `self` may be at the begining of a declaration.
  public var mayBeginDecl: Bool {
    switch kind {
    case .extension, .deinit, .fun, .`init`, .inout, .let, .namespace, .type, .trait, .var:
      return true
    default:
      return isDeclModifier
    }
  }

  /// Indicates whether `self` may be at the begining of a control statement.
  public var mayBeginCtrlStmt: Bool {
    switch kind {
    case .break, .continue, .for, .if, .lBrace, .return, .while:
      return true
    default:
      return false
    }
  }

  /// Returns whether `self.kind` is contained in `kinds`.
  ///
  /// - Parameter kinds: A list of kinds.
  public func isOf<T: Collection>(kind kinds: T) -> Bool where T.Element == Kind {
    kinds.contains(kind)
  }

}
