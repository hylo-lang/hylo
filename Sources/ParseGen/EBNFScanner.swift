import Utils
import CitronLexerModule

let tokenID: [Character: EBNF.Token.ID] = [
  "*": .STAR,
  "+": .PLUS,
  "(": .LPAREN,
  ")": .RPAREN,
  "?": .QUESTION
]

struct Input {
  var tail: Substring
  var currentLine: Int
  var column1: String.Index


  init(_ text: Substring, onLine startLine: Int) {
    column1 = text.startIndex
    currentLine = startLine
    tail = text
  }

  mutating func skipWhitespace() {
    while !tail.isEmpty && tail.first!.isWhitespace { popFirst() }
  }

  mutating func skipHorizontalSpace() {
    tail = tail.drop { c in c.isWhitespace && !c.isNewline }
  }

  mutating func eatNonWhitespace() -> Substring? {
    let h = tail
    tail = tail.drop { !$0.isWhitespace }
    return h.startIndex == tail.startIndex ? nil : h[..<tail.startIndex]
  }

  mutating func eatSymbolName() -> Substring? {
    guard let c = tail.first, c.isLetter else { return nil }
    let h = tail
    tail = tail.drop { $0.isLetter || $0.isNumber || $0 == "-" || $0 == "_" }
    return h[..<tail.startIndex]
  }

  mutating func eatQuotedLiteral() -> Substring? {
    guard let c = tail.first, c == "'" else { return nil }
    var s = self
    s.popFirst()
    while let c = s.first, c != "'" {
      if c == "\\" { s.popFirst() }
      s.popFirst()
    }
    if s.popFirst() != "'" { return nil }
    defer { self = s }
    return tail[..<s.tail.startIndex]
  }

  mutating func eat(_ target: String) -> Substring? {
    var t = target[...]
    var s = self
    while t.first == s.tail.first {
      if t.isEmpty {
        defer { self = s }
        return tail[..<s.tail.startIndex]
      }
      t = t.dropFirst()
      s.popFirst()
    }
    return nil
  }

  mutating func eatLine() -> Substring {
    let h = tail
    tail = tail.drop { !$0.isNewline }
    return h[..<tail.startIndex]
  }

  @discardableResult
  mutating func popFirst() -> Character? {
    guard let r = tail.popFirst() else { return nil }
    if r.isNewline {
      currentLine += 1
      column1 = tail.startIndex
    }
    return r
  }

  var isEmpty: Bool { tail.isEmpty }

  var first: Character? { tail.first }
}

extension EBNF {
  func tokens(
    in sourceText: Substring, onLine startLine: Int, fromFile sourcePath: String
  ) -> [Token] {
    var output: [Token] = []
    var input = Input(sourceText, onLine: startLine)

    func token(_ kind: EBNF.Token.ID, _ text: Substring) {
      let startColumn = sourceText.distance(from: input.column1, to: text.startIndex) + 1

      let start = SourcePosition(line: input.currentLine, column: startColumn)
      let end = SourcePosition(line: input.currentLine, column: startColumn + text.count)

      output.append(Token(kind, text, at: SourceRegion(fileName: sourcePath, start..<end)))
    }

    func illegalToken() {
      characterToken(.ILLEGAL_CHARACTER)
    }

    func characterToken(_ id: EBNF.Token.ID) {
      token(id, input.tail.prefix(1))
      input.popFirst()
    }

    while true {
      input.skipWhitespace()

      var currentRuleKind: Rule.Kind = .plain

      while let c = input.first, !c.isNewline {
        if let s = input.eatSymbolName() {
          token(.SYMBOL_NAME, s)
        }
        else if let t = input.eat("::=") {
          token(.IS_DEFINED_AS, t)
        }
        else if let t = input.eat("(one of)") {
          token(.ONE_OF_KIND, t)
          currentRuleKind = .oneOf
        }
        else if let t = input.eat("(token)") {
          token(.TOKEN_KIND, t)
          currentRuleKind = .token
        }
        else if let t = input.eat("(regexp)") {
          token(.REGEXP_KIND, t)
          currentRuleKind = .regexp
        }
        else if input.isEmpty {
          return output
        }
        else {
          illegalToken()
        }
        input.skipHorizontalSpace()
      }
      input.popFirst(); input.skipHorizontalSpace()

      while let c = input.first, !c.isNewline {
        // Process one line with its trailing newline and any leading space on the next line
        switch currentRuleKind {
        case .oneOf:
          while let x = input.eatNonWhitespace() {
            token(.LITERAL, x)
            input.skipHorizontalSpace()
          }

        case .plain, .token:
          while true {
            if let t = input.eatQuotedLiteral() {
              token(.QUOTED_LITERAL, t)
            }
            else if let t = input.eatSymbolName() {
              token(.SYMBOL_NAME, t)
            }
            else if let c = input.first, let k = tokenID[c] {
              characterToken(k)
            }
            else { illegalToken() }
            input.skipHorizontalSpace()
          }

        case .regexp:
          let l = input.eatLine()
          token(.REGEXP, l.strippingWhitespace())
        }
        input.popFirst(); input.skipHorizontalSpace()
      }
    }

    return output
  }
}
