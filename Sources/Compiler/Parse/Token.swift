/// A terminal symbol of the syntactic grammar.
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
    case `catch`
    case `conformance`
    case `continue`
    case `deinit`
    case `else`
    case `extension`
    case `for`
    case `fun`
    case `if`
    case `import`
    case `in`
    case `indirect`
    case `init`
    case `inout`
    case `let`
    case `match`
    case `namespace`
    case `nil`
    case `operator`
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

  }

  /// The kind of the token.
  public var kind: Kind

  /// The range of the token in the source file from which it has been parsed.
  public var range: SourceRange

  /// Indicates whether `self` is a keyword.
  public var isKeyword: Bool {
    (Kind.async.rawValue ..< Kind.async.rawValue + 999) ~= kind.rawValue
  }

  /// Indicates whether `self` is a suitable as a label.
  public var isLabel: Bool {
    (kind == .name) || isKeyword
  }

  /// Indicates whether `self` is a suitable as an operator.
  ///
  /// Use this property rather than testing `self.kind == .oper`. The assignment operator (i.e.,
  /// `=`) and the angle brackets (i.e., `<` and `>`) do not have the kind `.oper`.
  public var isOperator: Bool {
    isOf(kind: [.oper, .assign, .lAngle, .rAngle])
  }

  /// Indicates whether `self` is a declaration modifier.
  public var isDeclModifier: Bool {
    switch kind {
    // case .infix, .postfix, .prefix: fallthrough
    case .public, .static:
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
