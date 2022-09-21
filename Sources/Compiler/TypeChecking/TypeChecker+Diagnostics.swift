extension Diagnostic {

  static func ambiguousDisjunction(range: SourceRange?) -> Diagnostic {
    .error("ambiguous disjunction", range: range)
  }

  static func ambiguousTypeReference(name: String, range: SourceRange?) -> Diagnostic {
    .error("ambiguous reference to type named \(name)", range: range)
  }

  static func circularRefinement(range: SourceRange?) -> Diagnostic {
    .error("circular trait refinement", range: range)
  }

  static func circularDependency(range: SourceRange?) -> Diagnostic {
    .error("circular dependency", range: range)
  }

  static func cannotConstruct(trait: TraitType, range: SourceRange?) -> Diagnostic {
    .error(
      "cannot construct an instance of trait '\(trait)'; did you mean 'any \(trait)'?",
      range: range)
  }

  static func cannotInferComplexReturnType(range: SourceRange?) -> Diagnostic {
    .error(
      "cannot infer complex return type; add an explicit return type annotation",
      range: range)
  }

  static func conformanceToNonTraitType(_ type: Type, range: SourceRange?) -> Diagnostic {
    .error("conformance to non-trait type '\(type)'", range: range)

  }

  static func declarationRequiresBody(range: SourceRange?) -> Diagnostic {
    .error("declaration requires a body", range: range)
  }

  static func duplicateCaptureName(_ name: String, range: SourceRange?) -> Diagnostic {
    .error("duplicate capture name '\(name)'", range: range)
  }

  static func duplicateOperatorDeclaration(_ name: String, range: SourceRange?) -> Diagnostic {
    .error("duplicate operator declaration '\(name)'", range: range)
  }

  static func duplicateParameterName(_ name: String, range: SourceRange?) -> Diagnostic {
    .error("duplicate parameter name '\(name)'", range: range)
  }

  static func genericDeclHasCaptures(range: SourceRange?) -> Diagnostic {
    .error("generic declaration has captures", range: range)
  }

  static func illegalMemberwiseInit(range: SourceRange?) -> Diagnostic {
    .error(
      "memberwise initializer declaration may only appear in product type declaration",
      range: range)
  }

  static func illegalParameterConvention(
    _ convention: PassingConvention,
    range: SourceRange?
  ) -> Diagnostic {
    .error("'\(convention)' may only be used on parameters", range: range)
  }

  static func implicitReferenceToForeignReceiver(range: SourceRange?) -> Diagnostic {
    .error("implicit reference to foreign receiver", range: range)
  }

  static func incompatibleLabels(
    found: [String?],
    expected: [String?],
    range: SourceRange?
  ) -> Diagnostic {
    let ls = found.reduce(into: "", { (string, label) in string += (label ?? "_") + ":" })
    let rs = expected.reduce(into: "", { (string, label) in string += (label ?? "_") + ":" })
    return .error("incompatible labels: found '(\(ls))', expected '(\(rs))'", range: range)
  }

  static func incompatibleParameterCount(range: SourceRange?) -> Diagnostic {
    .error("incompatible number of parameters", range: range)
  }

  static func incompatibleTupleLengths(range: SourceRange?) -> Diagnostic {
    .error("tuples have different lengths", range: range)
  }

  static func incompatibleTypes(_ l: Type, _ r: Type, range: SourceRange?) -> Diagnostic {
    .error("incompatible types '\(l)' and '\(r)'", range: range)
  }

  static func invalidAssociatedTypeExpr(_ name: String, range: SourceRange?) -> Diagnostic {
    .error(
      """
      associated type '\(name)' can only be used referred to as a member of a generic type \
      parameter, a conformance lens, or another associated type
      """,
      range: range)
  }

  static func invalidClosureParameterCount(
    expected: Int,
    found: Int,
    range: SourceRange?
  ) -> Diagnostic {
    .error(
      "contextual closure type requires \(expected) argument(s), found \(found)",
      range: range)
  }

  static func invalidInoutBundleReturnType(expected: Type, range: SourceRange?) -> Diagnostic {
    .error("inout-capable method bundle must return '\(expected)'", range: range)
  }

  static func invalidDestructuring(ofType type: Type, range: SourceRange?) -> Diagnostic {
    .error("invalid destructuring of type '\(type)'", range: range)
  }

  static func invalidSelfTypeExpr(range: SourceRange?) -> Diagnostic {
    .error("reference to 'Self' outside of a type context", range: range)
  }

  static func invalidParameterType(_ type: Type, range: SourceRange?) -> Diagnostic {
    .error("invalid parameter type '\(type)'", range: range)
  }

  static func memberDeclHasCaptures(range: SourceRange?) -> Diagnostic {
    .error("member declaration has captures", range: range)
  }

  static func missingReturnValue(range: SourceRange?) -> Diagnostic {
    .error("non-unit function should return a value", range: range)
  }

  static func missingTypeAnnotation(range: SourceRange?) -> Diagnostic {
    .error("missing type annotation", range: range)
  }

  static func nestedOperatorDeclaration(range: SourceRange?) -> Diagnostic {
    .error("operator declaration can only appear at top-level", range: range)
  }

  static func nonTraitType(_ type: Type, range: SourceRange?) -> Diagnostic {
    .error("type '\(type)' is not a trait", range: range)
  }

  static func noConformance(
    of type: Type,
    to trait: TraitType,
    range: SourceRange?
  ) -> Diagnostic {
    .error("type '\(type)' does not conform to trait '\(trait)'", range: range)
  }

  static func noSkolemInConformance(_ type: Type, range: SourceRange?) -> Diagnostic {
    .error(
      """
      type '\(type)' in conformance constraint does not refers to a generic parameter or \
      associated type
      """,
      range: range)
  }

  static func noSkolemInEquality(l: Type, r: Type, range: SourceRange?) -> Diagnostic {
    .error(
      """
      neither type in equality constraint ('\(l)' or '\(r)') refers to a generic parameter or \
      associated type
      """,
      range: range)
  }

  static func notEnoughContextToInferArguments(range: SourceRange?) -> Diagnostic {
    .error(
      "not enough contextual information to infer the arguments to generic parameters",
      range: range)
  }

  static func notSubtype(_ l: Type, of r: Type, range: SourceRange?) -> Diagnostic {
    .error("'\(l)' is not subtype of '\(r)'", range: range)
  }

  static func noType(
    named name: String,
    in domain: Type? = nil,
    range: SourceRange?
  ) -> Diagnostic {
    if let domain = domain {
      return .error("type '\(domain)' has no type member '\(name)'", range: range)
    } else {
      return .error("no type named '\(name)' in this scope", range: range)
    }
  }

  static func staleConstraint(constraint: Constraint, range: SourceRange?) -> Diagnostic {
    .error("stale constraint '\(constraint)'", range: range)
  }

  static func staticMemberUsedOnInstance(
    member: Name,
    type: Type,
    range: SourceRange?
  ) -> Diagnostic {
    .error(
      "static member '\(member)' cannot be used on instance of type '\(type)'",
      range: range)
  }

  static func undefined(name: String, range: SourceRange?) -> Diagnostic {
    .error("undefined name '\(name)' in this scope", range: range)
  }

  static func undefinedOperator(_ name: String, range: SourceRange?) -> Diagnostic {
    .error("undefined operator '\(name)'", range: range)
  }

  static func unusedResult(ofType type: Type, range: SourceRange?) -> Diagnostic {
    .warning("unused result of type '\(type)'", range: range)
  }

}
