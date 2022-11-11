import Utils

extension Diagnostic {

  static func diagnose(
    duplicateAccessModifier m: SourceRepresentable<AccessModifier>
  ) -> Diagnostic {
    .error(
      "duplicate access modifier '\(m.value)'",
      range: m.range)
  }

  static func diagnose(
    duplicateImplementationIntroducer i: SourceRepresentable<ImplIntroducer>
  ) -> Diagnostic {
    .error(
      "duplicate implementation introducer '\(i.value)'",
      range: i.range)
  }

  static func diagnose(
    duplicateMemberModifier m: SourceRepresentable<MemberModifier>
  ) -> Diagnostic {
    .error(
      "duplicate member modifier '\(m.value)'",
      range: m.range)
  }

  static func diagnose(
    illegalGlobalBindingIntroducer i: SourceRepresentable<BindingPattern.Introducer>
  ) -> Diagnostic {
    .error(
      "global binding must be introduced by 'let'",
      range: i.range)
  }

  static func diagnose(
    illegalMemberBindingIntroducer i: SourceRepresentable<BindingPattern.Introducer>
  ) -> Diagnostic {
    .error(
      "member binding must be introduced by 'let' or 'var'",
      range: i.range)
  }

  static func diagnose(
    invalidOperatorLabels found: [String?],
    expected: [String?],
    at range: SourceRange?
  ) -> Diagnostic {
    .error(
      """
      invalid operator labels '\(Name.describe(labels: found))', \
      expected '\(Name.describe(labels: found))'
      """,
      range: range)
  }

  static func diagnose(
    invalidOperatorNotation found: OperatorNotation,
    expected: OperatorNotation,
    at range: SourceRange?
  ) -> Diagnostic {
    .error(
      "invalid operator notation '\(found)', expected '\(expected)'",
      range: range)
  }

  static func diagnose(
    memberModifier member: SourceRepresentable<MemberModifier>,
    appearsBeforeAccessModifier access: SourceRepresentable<AccessModifier>
  ) -> Diagnostic {
    return .error(
      "member modifier '\(member.value)' must appear after access modifier '\(access.value)'",
      range: member.range)
  }

  static func diagnose(
    missingFunctionIdentifier d: FunctionDecl
  ) -> Diagnostic {
    .error(
      "missing identifier in function declaration",
      range: d.introducerRange)
  }

  static func diagnose(
    missingMethodIdentifier d: MethodDecl
  ) -> Diagnostic {
    diagnose(missingMethodIdentifierAt: d.introducerRange)
  }

  static func diagnose(
    missingMethodIdentifierAt range: SourceRange?
  ) -> Diagnostic {
    .error(
      "missing identifier in method bundle declaration",
      range: range)
  }

  static func diagnose(
    missingSubscriptIdentifier d: SubscriptDecl
  ) -> Diagnostic {
    .error(
      "missing identifier in subscript declaration",
      range: d.introducer.range)
  }

  static func diagnose(
    missingTypeAnnotation p: BindingPattern,
    in ast: AST
  ) -> Diagnostic {
    .error(
      "missing type annotation",
      range: ast.ranges[p.subpattern])
  }

  static func diagnose(
    missingTypeAnnotation p: ParameterDecl,
    in ast: AST
  ) -> Diagnostic {
    .error(
      "missing type annotation",
      range: p.identifier.range)
  }

  static func diagnose(
    unexpectedAccessModifier m: SourceRepresentable<AccessModifier>
  ) -> Diagnostic {
    .error(
      "unexpected access modifier '\(m.value)'",
      range: m.range)
  }

  static func diagnose(
    unexpectedAssociatedTypeDecl d: AssociatedTypeDecl
  ) -> Diagnostic {
    .error(
      "associated type declaration is not allowed here",
      range: d.introducerRange)
  }

  static func diagnose(
    unexpectedAssociatedValueDecl d: AssociatedValueDecl
  ) -> Diagnostic {
    .error(
      "associated value declaration is not allowed here",
      range: d.introducerRange)
  }

  static func diagnose(
    unexpectedAttribute a: SourceRepresentable<Attribute>
  ) -> Diagnostic {
    .error(
      "unexpected attribute modifier '\(a.value.name.value)'",
      range: a.value.name.range)
  }

  static func diagnose(
    unexpectedCapture p: BindingPattern
  ) -> Diagnostic {
    .error(
      "explicit capture is not allowed here",
      range: p.introducer.range)
  }

  static func diagnose(
    unexpectedImportDecl d: ImportDecl
  ) -> Diagnostic {
    .error(
      "import declaration is not allowed here",
      range: d.introducerRange)
  }

  static func diagnose(
    unexpectedGenericTypeParameterDecl d: GenericTypeParamDecl
  ) -> Diagnostic {
    .error(
      "generic type parameter declaration is not allowed here",
      range: d.identifier.range)
  }

  static func diagnose(
    unexpectedGenericValueParameterDecl d: GenericValueParamDecl
  ) -> Diagnostic {
    .error(
      "generic value parameter declaration is not allowed here",
      range: d.identifier.range)
  }

  static func diagnose(
    unexpectedInitializerDecl d: InitializerDecl
  ) -> Diagnostic {
    .error(
      "initializer declaration is not allowed here",
      range: d.introducer.range)
  }

  static func diagnose(
    unexpectedMemberModifier m: SourceRepresentable<MemberModifier>
  ) -> Diagnostic {
    .error(
      "unexpected member modifier '\(m.value)'",
      range: m.range)
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
      range: d.introducer.range)
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
      range: d.identifier.range)
  }

  static func diagnose(
    unexpectedPropertyDecl d: SubscriptDecl
  ) -> Diagnostic {
    .error(
      "property declaration is not allowed here",
      range: d.introducer.range)
  }

  static func diagnose(
    unexpectedEffect e: SourceRepresentable<ReceiverEffect>
  ) -> Diagnostic {
    .error(
      "unexpected effect '\(e.value)'",
      range: e.range)
  }

  static func diagnose(
    unexpectedSubscriptImplDecl d: SubscriptImplDecl
  ) -> Diagnostic {
    .error(
      "subscript implementation declaration is not allowed here",
      range: d.introducer.range)
  }

  static func diagnose(
    unexpectedTraitDecl d: TraitDecl
  ) -> Diagnostic {
    .error(
      "trait declaration is not allowed here",
      range: d.identifier.range)
  }

  static func diagnose(
    unexpectedVarDecl d: VarDecl
  ) -> Diagnostic {
    .error(
      "variable declaration is not allowed here",
      range: d.identifier.range)
  }

}
