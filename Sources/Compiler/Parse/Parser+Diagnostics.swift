extension Diagnostic {

  static func diagnose(assignOperatorRequiresWhitespaces token: Token) -> Diagnostic {
    .error("assignment operator requires whitespaces on both sides", range: token.origin)
  }

  static func diagnose(expected kind: Token.Kind, at location: SourceLocation) -> Diagnostic {
    diagnose(expected: "'\(kind)'", at: location)
  }

  static func diagnose(
    expected subject: String,
    at location: SourceLocation,
    children: [Diagnostic] = []
  ) -> Diagnostic {
    .error("expected \(subject)", range: location ..< location, children: children)
  }

  static func diagnose(
    expected closerDescription: String,
    matching opener: Token,
    in state: ParserState
  ) -> Diagnostic {
    .diagnose(
      expected: "'\(closerDescription)'",
      at: state.currentLocation,
      children: [
        .error(
          "to match this '\(state.lexer.source[opener.origin])'",
          range: opener.origin)
      ]
    )
  }

  static func diagnose(infixOperatorRequiresWhitespacesAt range: SourceRange?) -> Diagnostic {
    .error("infix operator requires whitespaces on both sides", range: range)
  }

  static func diagnose(separatedMutationMarkerAt range: SourceRange?) -> Diagnostic {
    .error("in-place mutation marker cannot be separated from its expression", range: range)
  }

  static func diagnose(separatedPrefixOperatorAt range: SourceRange?) -> Diagnostic {
    .error("prefix operator cannot be separated from its operand", range: range)
  }

  static func diagnose(unexpectedToken token: Token) -> Diagnostic {
    .error("unexpected token '\(token.kind)'", range: token.origin)
  }

  static func diagnose(unterminatedCommentEndingAt endLocation: SourceLocation) -> Diagnostic {
    .error("unterminated comment", range: endLocation ..< endLocation)
  }

  static func diagnose(unterminatedStringEndingAt endLocation: SourceLocation) -> Diagnostic {
    .error("unterminated string", range: endLocation ..< endLocation)
  }

}
