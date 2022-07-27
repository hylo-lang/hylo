import DequeModule
import Durian

/// A type representing the context of the parser.
struct ParserContext {

  /// The flags of a parser's state.
  struct Flags {

    private var rawValue: UInt16

    subscript(flags: Flags) -> Bool {
      (rawValue & flags.rawValue) != 0
    }

    static func | (lhs: Flags, rhs: Flags) -> Flags {
      Flags(rawValue: lhs.rawValue | rhs.rawValue)
    }

    static let parsingTopLevel        = Flags(rawValue: 1 << 0)
    static let parsingNamespace       = Flags(rawValue: 1 << 1)
    static let parsingProductBody     = Flags(rawValue: 1 << 2)
    static let parsingTraitBody       = Flags(rawValue: 1 << 3)
    static let parsingExtensionBody   = Flags(rawValue: 1 << 4)
    static let parsingFunctionBody    = Flags(rawValue: 1 << 5)
    static let parsingSubscriptBody   = Flags(rawValue: 1 << 6)
    static let parsingBindingPattern  = Flags(rawValue: 1 << 7)
    static let parsingLoopBody        = Flags(rawValue: 1 << 8)

    static let parsingTypeBody = parsingProductBody | parsingTraitBody | parsingExtensionBody

  }

  /// The AST being parsed.
  var ast: AST

  /// The lexer generating the tokens to parse.
  var lexer: Lexer

  /// The current index of the parser in the character stream.
  var currentIndex: String.Index

  /// The lookahead buffer.
  var lookahead = Deque<Token>()

  /// The diagnostics of the parse errors and warnings.
  var diagnostics: [Diagnostic] = []

  /// The flags of the parser's state.
  var flags = Flags.parsingTopLevel

  /// Indicates that the parser encounted an irrecoverable error.
  var didIrrecoverableErrorOccur = false

  /// Creates a new context, using `lexer` to generate tokens.
  init(ast: AST, lexer: Lexer) {
    self.ast = ast
    self.lexer = lexer
    self.currentIndex = lexer.source.contents.startIndex
  }

  /// The current location of the parser in the character stream.
  var currentLocation: SourceLocation { SourceLocation(source: lexer.source, index: currentIndex) }

  /// The next character in the charachter stream, unless the parser reached its end.
  var currentCharacter: Character? { isAtEOF ? nil : lexer.source.contents[currentIndex] }

  /// Returns whether the parser is at the end of the character stream.
  var isAtEOF: Bool { currentIndex == lexer.source.contents.endIndex }

  /// Returns whether there is a whitespace at the current index.
  var hasLeadingWhitespace: Bool {
    currentIndex < lexer.source.contents.endIndex
      && lexer.source.contents[currentIndex].isWhitespace
  }

  /// Returns whether there is a new line character in the character stream from the current index
  /// up to but not including the specified index.
  func hasNewline(inCharacterStreamUpTo bound: String.Index) -> Bool {
    lexer.source.contents[currentIndex ..< bound].contains(where: { $0.isNewline })
  }

  /// Returns whether `token` is a member index.
  func isMemberIndex(_ token: Token) -> Bool {
    (token.kind == .int) && lexer.source[token.range].allSatisfy({ ch in
      guard let ascii = ch.asciiValue else { return false }
      return (0x30 ... 0x39) ~= ascii
    })
  }

  /// Returns the next token without consuming it, if any.
  mutating func peek() -> Token? {
    // Return the token in the lookahead buffer, if available.
    if let token = lookahead.first {
      return token
    }

    // Attempt to pull a new element from the lexer.
    guard let token = lexer.next() else { return nil }
    lookahead.append(token)
    return token
  }

  /// Consumes and returns the next token, if any.
  mutating func take() -> Token? {
    // Return the token in the lookahead buffer, if available.
    if let token = lookahead.popFirst() {
      currentIndex = token.range.upperBound
      return token
    }

    // Attempt to pull a new element from the lexer.
    guard let token = lexer.next() else { return nil }
    currentIndex = token.range.upperBound
    return token
  }

  /// Consumes and returns the next token, only if it has the specified kind.
  mutating func take(_ kind: Token.Kind) -> Token? {
    if peek()?.kind == kind {
      let token = lookahead.removeFirst()
      currentIndex = token.range.upperBound
      return token
    } else {
      return nil
    }
  }

  /// Consumes and returns the next token, only if it has the specified kind and if it is not
  /// preceeded by any whitespace.
  mutating func takeWithoutSkippingWhitespace(_ kind: Token.Kind) -> Token? {
    if hasLeadingWhitespace { return nil }
    return take(kind)
  }

  /// Consumes and returns the next token, if it has the specified predicate.
  mutating func take(if predicate: (Token) -> Bool) -> Token? {
    if let token = peek(), predicate(token) {
      let token = lookahead.removeFirst()
      currentIndex = token.range.upperBound
      return token
    } else {
      return nil
    }
  }

  /// Consumes and returns a name token with the specified value.
  mutating func take(nameTokenWithValue value: String) -> Token? {
    take(if: { [source = lexer.source] in
      ($0.kind == .name) && (source[$0.range] == value)
    })
  }

  /// Consumes and returns an operator (excluding `=`) from the token stream.
  ///
  /// If the next token in the stream is an angle bracket, it is interpreter as an operator and
  /// merged with any attached operator.
  mutating func takeOperator() -> SourceRepresentable<Identifier>? {
    guard let head = peek() else { return nil }

    switch head.kind {
    case .oper, .ampersand, .equal, .pipe:
      _ = take()
      return SourceRepresentable(value: String(lexer.source[head.range]), range: head.range)

    case .lAngle, .rAngle:
      // Operator starts with `<` or `>`.
      _ = take()

      // Merge the leading angle bracket with attached operators.
      var upper = head.range.upperBound
      while let next = take(if: { $0.isOperatorToken && (upper == $0.range.lowerBound) }) {
        upper = next.range.upperBound
      }

      var range = head.range
      range.upperBound = upper
      return SourceRepresentable(value: String(lexer.source[range]), range: range)

    default:
      return nil
    }
  }

  /// Consumes and returns the value of a member index.
  mutating func takeMemberIndex() -> Int? {
    if let index = take(if: isMemberIndex(_:)) {
      return Int(lexer.source[index.range])!
    } else {
      return nil
    }
  }

}

extension ParserContext: Restorable {

  typealias Backup = Self

}
