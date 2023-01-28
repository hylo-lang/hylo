import Core

extension Diagnostic {

  static func error(assignOperatorRequiresWhitespaces token: Token) -> Diagnostic {
    .error("assignment operator requires whitespaces on both sides", at: token.site)
  }

  static func error(expected kind: Token.Kind, at location: SourcePosition) -> Diagnostic {
    error(expected: "'\(kind)'", at: location)
  }

  static func error(expected subject: String, at location: SourcePosition, notes: [Diagnostic] = [])
    -> Diagnostic
  {
    .error("expected \(subject)", at: location ..< location, notes: notes)
  }

  static func error(
    expected closerDescription: String, matching opener: Token, in state: ParserState
  ) -> Diagnostic {
    .error(
      expected: "'\(closerDescription)'", at: state.currentLocation,
      notes: [
        .error("to match this '\(state.lexer.sourceCode[opener.site])'", at: opener.site)
      ])
  }

  static func error(infixOperatorRequiresWhitespacesAt site: SourceRange) -> Diagnostic {
    .error("infix operator requires whitespaces on both sides", at: site)
  }

  static func error(separatedMutationMarkerAt site: SourceRange) -> Diagnostic {
    .error("in-place mutation marker cannot be separated from its expression", at: site)
  }

  static func error(separatedPrefixOperatorAt site: SourceRange) -> Diagnostic {
    .error("prefix operator cannot be separated from its operand", at: site)
  }

  static func error(unexpectedToken token: Token) -> Diagnostic {
    .error("unexpected token '\(token.kind)'", at: token.site)
  }

  static func error(unterminatedCommentEndingAt endLocation: SourcePosition) -> Diagnostic {
    .error("unterminated comment", at: endLocation ..< endLocation)
  }

  static func error(unterminatedStringEndingAt endLocation: SourcePosition) -> Diagnostic {
    .error("unterminated string", at: endLocation ..< endLocation)
  }

  static func error(duplicateAccessModifier m: SourceRepresentable<AccessModifier>) -> Diagnostic {
    .error("duplicate access modifier '\(m.value)'", at: m.site)
  }

  static func error(duplicateImplementationIntroducer i: SourceRepresentable<ImplIntroducer>)
    -> Diagnostic
  {
    .error("duplicate implementation introducer '\(i.value)'", at: i.site)
  }

  static func error(duplicateMemberModifier m: SourceRepresentable<MemberModifier>) -> Diagnostic {
    .error("duplicate member modifier '\(m.value)'", at: m.site)
  }

  static func error(
    memberModifier member: SourceRepresentable<MemberModifier>,
    appearsBeforeAccessModifier access: SourceRepresentable<AccessModifier>
  ) -> Diagnostic {
    return .error(
      "member modifier '\(member.value)' must appear after access modifier '\(access.value)'",
      at: member.site)
  }

  static func error(unexpectedAccessModifier m: SourceRepresentable<AccessModifier>) -> Diagnostic {
    .error("unexpected access modifier '\(m.value)'", at: m.site)
  }

  static func error(unexpectedAttribute a: SourceRepresentable<Attribute>) -> Diagnostic {
    .error("unexpected attribute modifier '\(a.value.name.value)'", at: a.value.name.site)
  }

  static func error(unexpectedInitializerDecl d: InitializerDecl) -> Diagnostic {
    .error("initializer declaration is not allowed here", at: d.introducer.site)
  }

  static func error(unexpectedMemberModifier m: SourceRepresentable<MemberModifier>) -> Diagnostic {
    .error("unexpected member modifier '\(m.value)'", at: m.site)
  }

  static func error(unexpectedMethodDecl d: MethodDecl) -> Diagnostic {
    .error("method bundle declaration is not allowed here", at: d.introducerSite)
  }

  static func error(unexpectedMethodImplDecl d: MethodImplDecl) -> Diagnostic {
    .error("method implementation declaration is not allowed here", at: d.introducer.site)
  }

  static func error(unexpectedNamespaceDecl d: NamespaceDecl) -> Diagnostic {
    .error("namespace declaration is not allowed here", at: d.introducerSite)
  }

  static func error(unexpectedOperatorDecl d: OperatorDecl) -> Diagnostic {
    .error("operator declaration is not allowed here", at: d.introducerSite)
  }

  static func error(unexpectedParameterDecl d: ParameterDecl) -> Diagnostic {
    .error("parameter declaration is not allowed here", at: d.identifier.site)
  }

  static func error(unexpectedPropertyDecl d: SubscriptDecl) -> Diagnostic {
    .error("property declaration is not allowed here", at: d.introducer.site)
  }

  static func error(unexpectedEffect e: SourceRepresentable<AccessEffect>) -> Diagnostic {
    .error("unexpected effect '\(e.value)'", at: e.site)
  }

  static func error(unexpectedSubscriptImplDecl d: SubscriptImplDecl) -> Diagnostic {
    .error("subscript implementation declaration is not allowed here", at: d.introducer.site)
  }

  static func error(unexpectedTraitDecl d: TraitDecl) -> Diagnostic {
    .error("trait declaration is not allowed here", at: d.identifier.site)
  }

  static func error(unexpectedVarDecl d: VarDecl) -> Diagnostic {
    .error("variable declaration is not allowed here", at: d.identifier.site)
  }

  static func error(nestedBindingPattern p: NodeID<BindingPattern>, in ast: AST) -> Diagnostic {
    .error(
      "'\(ast[p].introducer.value)' cannot appear nested in another binding pattern",
      at: ast[p].introducer.site)
  }

}
