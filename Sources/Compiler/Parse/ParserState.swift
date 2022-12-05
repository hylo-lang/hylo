import DequeModule
import Durian

/// A type representing the state of the parser.
struct ParserState {

  /// A tag representing the context of the parser.
  enum Context {

    case bindingPattern

    case captureList

    case extensionBody

    case functionBody

    case loopBody

    case namespaceBody

    case productBody

    case subscriptBody

    case topLevel

    case traitBody

  }

  /// The AST being parsed.
  var ast: AST

  /// The lexer generating the tokens to parse.
  private(set) var lexer: Lexer

  /// The current index of the parser in the character stream.
  private(set) var currentIndex: String.Index

  /// The lookahead buffer.
  private var lookahead = Deque<Token>()

  /// The diagnostics of the parse errors and warnings.
  var diagnostics: [Diagnostic] = []

  /// A stack describing the parsing context.
  var contexts: [Context] = []

  /// Creates a new context, using `lexer` to generate tokens.
  init(ast: AST, lexer: Lexer) {
    self.ast = ast
    self.lexer = lexer
    self.currentIndex = lexer.source.contents.startIndex
  }

  /// The character stream from the current index.
  var characterStream: Substring { lexer.source.contents[currentIndex...] }

  /// Indicates whether the parser is at global scope.
  var atGlobalScope: Bool { atModuleScope || atNamespaceScope }

  /// Indicates whether the parser is at module scope.
  var atModuleScope: Bool { contexts.isEmpty }

  /// Indicates whether the parser is at namespace scope.
  var atNamespaceScope: Bool { contexts.last == .namespaceBody }

  /// Indicates whether the parser is expecting to parse member declarations.
  var atTypeScope: Bool {
    if let c = contexts.last {
      return (c == .extensionBody) || (c == .productBody) || (c == .traitBody)
    } else {
      return false
    }
  }

  /// Indicates whether the parser is at trait scope.
  var atTraitScope: Bool { contexts.last == .traitBody }

  /// Indicates whether the parser is expecting to parse a capture declaration.
  var isParsingCaptureList: Bool { contexts.last == .captureList }

  /// The current location of the parser in the character stream.
  var currentLocation: SourceLocation { SourceLocation(source: lexer.source, index: currentIndex) }

  /// The next character in the character stream, unless the parser reached its end.
  var currentCharacter: Character? { atEOF ? nil : lexer.source.contents[currentIndex] }

  /// Returns whether the parser is at the end of the character stream.
  var atEOF: Bool { currentIndex == lexer.source.contents.endIndex }

  /// Returns whether there is a whitespace at the current index.
  var hasLeadingWhitespace: Bool {
    currentIndex < lexer.source.contents.endIndex
      && lexer.source.contents[currentIndex].isWhitespace
  }

  /// Returns whether there is a new line in the character stream before `bound`.
  mutating func hasNewline(before bound: Token) -> Bool {
    lexer.source.contents[currentIndex ..< bound.origin.lowerBound]
      .contains(where: { $0.isNewline })
  }

  /// Returns a source range from `startIndex` to `self.currentIndex`.
  func range(from startIndex: String.Index) -> SourceRange {
    SourceRange(in: lexer.source, from: startIndex, to: currentIndex)
  }

  /// Returns whether `token` is a member index.
  func isMemberIndex(_ token: Token) -> Bool {
    (token.kind == .int) && lexer.source[token.origin].allSatisfy({ ch in
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

  /// Returns whether the next token satisfies `predicate`, or `false` if the stream is empty.
  mutating func peekAndTest(_ predicate: (Token) -> Bool) -> Bool {
    if let token = peek() {
      return predicate(token)
    } else {
      return false
    }
  }

  /// Consumes and returns the next token, if any.
  mutating func take() -> Token? {
    // Return the token in the lookahead buffer, if available.
    if let token = lookahead.popFirst() {
      currentIndex = token.origin.upperBound
      return token
    }

    // Attempt to pull a new element from the lexer.
    guard let token = lexer.next() else { return nil }
    currentIndex = token.origin.upperBound
    return token
  }

  /// Consumes and returns the next token, only if it has the specified kind.
  mutating func take(_ kind: Token.Kind) -> Token? {
    if peek()?.kind == kind {
      let token = lookahead.removeFirst()
      currentIndex = token.origin.upperBound
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
      currentIndex = token.origin.upperBound
      return token
    } else {
      return nil
    }
  }

  /// Consumes and returns a name token with the specified value.
  mutating func take(nameTokenWithValue value: String) -> Token? {
    take(if: { [source = lexer.source] in
      ($0.kind == .name) && (source[$0.origin] == value)
    })
  }

  /// Consumes and returns an operator (excluding `=`) from the token stream.
  ///
  /// If the next token in the stream is an angle bracket, it is interpreted as an operator and
  /// merged with any attached operator.
  mutating func takeOperator() -> SourceRepresentable<Identifier>? {
    guard let head = peek() else { return nil }

    switch head.kind {
    case .oper, .ampersand, .equal, .pipe:
      _ = take()
      return SourceRepresentable(value: String(lexer.source[head.origin]), range: head.origin)

    case .lAngle, .rAngle:
      // Operator starts with `<` or `>`.
      _ = take()

      // Merge the leading angle bracket with attached operators.
      var upper = head.origin.upperBound
      while let next = take(if: { $0.isOperatorPart && (upper == $0.origin.lowerBound) }) {
        upper = next.origin.upperBound
      }

      var range = head.origin
      range.upperBound = upper
      return SourceRepresentable(value: String(lexer.source[range]), range: range)

    default:
      return nil
    }
  }

  /// Consumes and returns the value of a member index.
  mutating func takeMemberIndex() -> Int? {
    if let index = take(if: isMemberIndex(_:)) {
      return Int(lexer.source[index.origin])!
    } else {
      return nil
    }
  }

  /// Consumes and returns an attribute token with the specified name.
  mutating func take(attribute name: String) -> Token? {
    take(if: { [source = lexer.source] in
      ($0.kind == .attribute) && (source[$0.origin] == name)
    })
  }

  /// Consumes tokens as long as they satisfy `predicate`.
  mutating func skip(while predicate: (Token) -> Bool) {
    while take(if: predicate) != nil {}
  }

}

extension ParserState: Restorable {

  typealias Backup = Self

}
