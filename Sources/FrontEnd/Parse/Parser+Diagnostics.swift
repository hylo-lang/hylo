import Core

extension Diagnostic {

  static func diagnose(assignOperatorRequiresWhitespaces token: Token) -> Diagnostic {
    .error("assignment operator requires whitespaces on both sides", range: token.origin)
  }

  static func diagnose(expected kind: Token.Kind, at location: SourcePosition) -> Diagnostic {
    diagnose(expected: "'\(kind)'", at: location)
  }

  static func diagnose(
    expected subject: String,
    at location: SourcePosition,
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
          "to match this '\(state.lexer.sourceCode[opener.origin])'",
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

  static func diagnose(unterminatedCommentEndingAt endLocation: SourcePosition) -> Diagnostic {
    .error("unterminated comment", range: endLocation ..< endLocation)
  }

  static func diagnose(unterminatedStringEndingAt endLocation: SourcePosition) -> Diagnostic {
    .error("unterminated string", range: endLocation ..< endLocation)
  }

  static func diagnose(
    duplicateAccessModifier m: SourceRepresentable<AccessModifier>
  ) -> Diagnostic {
    .error(
      "duplicate access modifier '\(m.value)'",
      range: m.origin)
  }

  static func diagnose(
    duplicateImplementationIntroducer i: SourceRepresentable<ImplIntroducer>
  ) -> Diagnostic {
    .error(
      "duplicate implementation introducer '\(i.value)'",
      range: i.origin)
  }

  static func diagnose(
    duplicateMemberModifier m: SourceRepresentable<MemberModifier>
  ) -> Diagnostic {
    .error(
      "duplicate member modifier '\(m.value)'",
      range: m.origin)
  }

  static func diagnose(
    memberModifier member: SourceRepresentable<MemberModifier>,
    appearsBeforeAccessModifier access: SourceRepresentable<AccessModifier>
  ) -> Diagnostic {
    return .error(
      "member modifier '\(member.value)' must appear after access modifier '\(access.value)'",
      range: member.origin)
  }

  static func diagnose(
    unexpectedAccessModifier m: SourceRepresentable<AccessModifier>
  ) -> Diagnostic {
    .error(
      "unexpected access modifier '\(m.value)'",
      range: m.origin)
  }

  static func diagnose(
    unexpectedAttribute a: SourceRepresentable<Attribute>
  ) -> Diagnostic {
    .error(
      "unexpected attribute modifier '\(a.value.name.value)'",
      range: a.value.name.origin)
  }

  static func diagnose(
    unexpectedInitializerDecl d: InitializerDecl
  ) -> Diagnostic {
    .error(
      "initializer declaration is not allowed here",
      range: d.introducer.origin)
  }

  static func diagnose(
    unexpectedMemberModifier m: SourceRepresentable<MemberModifier>
  ) -> Diagnostic {
    .error(
      "unexpected member modifier '\(m.value)'",
      range: m.origin)
  }

  static func diagnose(
    unexpectedMethodDecl d: MethodDecl
  ) -> Diagnostic {
    .error(
      "method bundle declaration is not allowed here",
      range: d.introducerRange)
  }

  static func diagnose(
    unexpectedMethodImplDecl d: MethodImplDecl
  ) -> Diagnostic {
    .error(
      "method implementation declaration is not allowed here",
      range: d.introducer.origin)
  }

  static func diagnose(
    unexpectedNamespaceDecl d: NamespaceDecl
  ) -> Diagnostic {
    .error(
      "namespace declaration is not allowed here",
      range: d.introducerRange)
  }

  static func diagnose(
    unexpectedOperatorDecl d: OperatorDecl
  ) -> Diagnostic {
    .error(
      "operator declaration is not allowed here",
      range: d.introducerRange)
  }

  static func diagnose(
    unexpectedParameterDecl d: ParameterDecl
  ) -> Diagnostic {
    .error(
      "parameter declaration is not allowed here",
      range: d.identifier.origin)
  }

  static func diagnose(
    unexpectedPropertyDecl d: SubscriptDecl
  ) -> Diagnostic {
    .error(
      "property declaration is not allowed here",
      range: d.introducer.origin)
  }

  static func diagnose(
    unexpectedEffect e: SourceRepresentable<AccessEffect>
  ) -> Diagnostic {
    .error(
      "unexpected effect '\(e.value)'",
      range: e.origin)
  }

  static func diagnose(
    unexpectedSubscriptImplDecl d: SubscriptImplDecl
  ) -> Diagnostic {
    .error(
      "subscript implementation declaration is not allowed here",
      range: d.introducer.origin)
  }

  static func diagnose(
    unexpectedTraitDecl d: TraitDecl
  ) -> Diagnostic {
    .error(
      "trait declaration is not allowed here",
      range: d.identifier.origin)
  }

  static func diagnose(
    unexpectedVarDecl d: VarDecl
  ) -> Diagnostic {
    .error(
      "variable declaration is not allowed here",
      range: d.identifier.origin)
  }

}
