import Basic

/// The lexer for Val source code.
public struct Lexer: IteratorProtocol, Sequence {

  /// The source file being tokenized.
  public let source: SourceFile

  /// The current index in the source file.
  public private(set) var index: SourceFile.Index

  /// Creates a new lexer for the given source file.
  ///
  /// - Parameter source: A source file.
  public init(source: SourceFile) {
    self.source = source
    self.index = source.startIndex
  }

  public mutating func next() -> Token? {
    // Skip whitespaces and comments.
    while true {
      guard index < source.endIndex else {
        return nil
      }

      // Skip whitespaces.
      if source[index].isWhitespace {
        index = source.index(after: index)
        continue
      }

      // Skip line comments.
      if source[index...].starts(with: "//") {
        while (index < source.endIndex) && !source[index].isNewline {
          index = source.index(after: index)
        }
        continue
      }

      // Skip block comments.
      if source[index...].starts(with: "/*") {
        // Consume the block opener.
        let start = index
        index = source.index(index, offsetBy: 2)

        // Search for the end of the block.
        var open = 1
        while open > 0 {
          if source[index...].starts(with: "/*") {
            index = source.index(index, offsetBy: 2)
            open += 1
          } else if source[index...].starts(with: "*/") {
            index = source.index(index, offsetBy: 2)
            open -= 1
          } else if index < source.endIndex {
            index = source.index(after: index)
          } else {
            return Token(kind: .unterminatedBlockComment, range: start ..< index)
          }
        }

        // We found the end of the block.
        continue
      }

      // The next character must be part of a token.
      break
    }

    // Scan a new token.
    let head = source[index]
    var token = Token(kind: .invalid, range: index ..< index)

    // Scan names and keywords.
    if head.isLetter || (head == "_") {
      let word = take(while: { $0.isLetter || $0.isDigit || ($0 == "_") })
      token.range = token.range.lowerBound ..< index

      switch word {
      case "_"        : token.kind = .under
      case "async"    : token.kind = .async
      case "await"    : token.kind = .await
      case "break"    : token.kind = .break
      case "case"     : token.kind = .case
      case "consuming": token.kind = .consuming
      case "continue" : token.kind = .continue
      case "del"      : token.kind = .del
      case "else"     : token.kind = .else
      case "extn"     : token.kind = .extn
      case "false"    : token.kind = .bool
      case "for"      : token.kind = .for
      case "fun"      : token.kind = .fun
      case "if"       : token.kind = .if
      case "in"       : token.kind = .in
      case "infix"    : token.kind = .infix
      case "is"       : token.kind = .cast
      case "let"      : token.kind = .let
      case "local"    : token.kind = .local
      case "match"    : token.kind = .match
      case "mod"      : token.kind = .mod
      case "mut"      : token.kind = .mut
      case "new"      : token.kind = .new
      case "nil"      : token.kind = .nil
      case "postfix"  : token.kind = .postfix
      case "prefix"   : token.kind = .prefix
      case "pub"      : token.kind = .pub
      case "ret"      : token.kind = .ret
      case "static"   : token.kind = .static
      case "true"     : token.kind = .bool
      case "type"     : token.kind = .type
      case "var"      : token.kind = .var
      case "view"     : token.kind = .view
      case "volatile" : token.kind = .volatile
      case "where"    : token.kind = .where
      case "while"    : token.kind = .while

      case "as":
        if let c = peek(), (c == "?") || (c == "!") {
          index = source.index(after: index)
        }
        token.range = token.range.lowerBound ..< index
        token.kind = .cast

      default:
        token.kind = .name
      }

      return token
    }

    // Scan a back-quoted names.
    if head == "`" {
      index = source.index(after: index)

      if let c = peek(), c.isLetter {
        let i = index
        _ = take(while: { $0.isLetter || $0.isDigit || ($0 == "_") })

        if peek() == "`" {
          token.kind = .name
          token.range = source.index(after: token.range.lowerBound) ..< index
          index = source.index(after: index)
          return token
        } else {
          index = i
        }
      }

      token.range = token.range.lowerBound ..< index
      return token
    }

    // Scan numeric literls
    if head.isDigit {
      token.kind = .int

      // Check if the literal is non-decimal.
      if head == "0" {
        index = source.index(after: index)
        switch peek() {
        case "x":
          index = source.index(after: index)
          _ = take(while: { $0.isHexDigit || ($0 == "_") })
          token.range = token.range.lowerBound ..< index
          return token

        case "o":
          index = source.index(after: index)
          _ = take(while: { $0.isOctDigit || ($0 == "_") })
          token.range = token.range.lowerBound ..< index
          return token

        case "b":
          index = source.index(after: index)
          _ = take(while: { $0.isBinDigit || ( $0 == "_") })
          token.range = token.range.lowerBound ..< index
          return token

        default:
          break
        }
      }

      // Consume the integer part.
      _ = take(while: { $0.isDigit || ($0 == "_") })

      // Consume the floating-point part, if any.
      if peek() == "." {
        index = source.index(after: index)
        if (peek() != "_") && !take(while: { $0.isDigit || ($0 == "_") }).isEmpty {
          token.kind = .float
        } else {
          index = source.index(before: index)
        }
      }

      // Consume the exponent, if any.
      if let c = peek(), (c == "e") || (c == "E") {
        let i = index

        index = source.index(after: index)
        if (peek() == "+") || (peek() == "-") {
          index = source.index(after: index)
        }

        if (peek() != "_") && !take(while: { $0.isDigit || ($0 == "_") }).isEmpty {
          token.kind = .float
        } else {
          index = i
        }
      }

      token.range = token.range.lowerBound ..< index
      return token
    }

    // Scan character strings.
    if head == "\"" {
      index = source.index(after: index)

      var escape = false
      while index < source.endIndex {
        if (source[index] == "\"") && !escape {
          index = source.index(after: index)
          token.kind = .string
          token.range = token.range.lowerBound ..< index
          return token
        } else if source[index] == "\\" {
          index = source.index(after: index)
          escape = !escape
        } else {
          index = source.index(after: index)
          escape = false
        }
      }

      token.kind = .unterminatedString
      token.range = token.range.lowerBound ..< index
      return token
    }

    // Scan operators.
    if head.isOperator {
      let oper: Substring
      switch head {
      case "<", ">":
        // Leading angle brackets are tokenized individually, to parse generic clauses.
        index = source.index(after: index)
        oper = source[token.range.lowerBound ..< index]

      default:
        oper = take(while: { $0.isOperator })
      }

      token.range = token.range.lowerBound ..< index
      switch oper {
      case "<" : token.kind = .lAngle
      case ">" : token.kind = .rAngle
      case "->": token.kind = .arrow
      case "=" : token.kind = .assign
      default  : token.kind = .oper
      }

      return token
    }

    // Scan punctuation.
    switch head {
    case ".": token.kind = .dot
    case ",": token.kind = .comma
    case ";": token.kind = .semi
    case "(": token.kind = .lParen
    case ")": token.kind = .rParen
    case "{": token.kind = .lBrace
    case "}": token.kind = .rBrace
    case "[": token.kind = .lBrack
    case "]": token.kind = .rBrack

    case ":":
      // Scan double colons.
      if source[index...].starts(with: "::") {
        index = source.index(index, offsetBy: 2)
        token.kind = .twoColons
        token.range = token.range.lowerBound ..< index
        return token
      }

      // Fall back to a simple colon.
      token.kind = .colon

    default:
      break
    }

    // Scan an invalid token.
    index = source.index(after: index)
    token.range = token.range.lowerBound ..< index
    return token
  }

  /// Returns the next character in the stream, without consuming it.
  private func peek() -> Character? {
    guard index < source.endIndex else { return nil }
    return source[index]
  }

  /// Consumes the longest sequence of characters that satisfy the given predicate.
  private mutating func take(while predicate: (Character) -> Bool) -> Substring {
    let start = index
    while let ch = peek(), predicate(ch) {
      index = source.index(after: index)
    }

    return source[start ..< index]
  }

}
