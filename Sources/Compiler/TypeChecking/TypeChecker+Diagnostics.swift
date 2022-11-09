extension Diagnostic {

  static func ambiguousDisjunction(at range: SourceRange?) -> Diagnostic {
    .error("ambiguous disjunction", range: range)
  }

  static func ambiguousTypeReference(name: String, at range: SourceRange?) -> Diagnostic {
    .error("ambiguous reference to type named \(name)", range: range)
  }

  static func captureOfNonStaticMemberInNestedType(
    name: Name,
    at range: SourceRange?
  ) -> Diagnostic {
    .error("cannot capture non-static member '\(name)' in nested type", range: range)
  }

  static func circularRefinement(at range: SourceRange?) -> Diagnostic {
    .error("circular trait refinement", range: range)
  }

  static func circularDependency(at range: SourceRange?) -> Diagnostic {
    .error("circular dependency", range: range)
  }

  static func cannotConstruct(trait: TraitType, at range: SourceRange?) -> Diagnostic {
    .error(
      "cannot construct an instance of trait '\(trait)'; did you mean 'any \(trait)'?",
      range: range)
  }

  static func cannotInferComplexReturnType(at range: SourceRange?) -> Diagnostic {
    .error(
      "cannot infer complex return type; add an explicit return type annotation",
      range: range)
  }

  static func conformanceToNonTraitType(_ type: Type, at range: SourceRange?) -> Diagnostic {
    .error("conformance to non-trait type '\(type)'", range: range)
  }

  static func declarationRequiresBody(at range: SourceRange?) -> Diagnostic {
    .error("declaration requires a body", range: range)
  }

  static func duplicateCaptureName(_ name: String, at range: SourceRange?) -> Diagnostic {
    .error("duplicate capture name '\(name)'", range: range)
  }

  static func duplicateOperatorDeclaration(_ name: String, at range: SourceRange?) -> Diagnostic {
    .error("duplicate operator declaration '\(name)'", range: range)
  }

  static func duplicateParameterName(_ name: String, at range: SourceRange?) -> Diagnostic {
    .error("duplicate parameter name '\(name)'", range: range)
  }

  static func genericDeclHasCaptures(at range: SourceRange?) -> Diagnostic {
    .error("generic declaration has captures", range: range)
  }

  static func illegalMemberwiseInit(at range: SourceRange?) -> Diagnostic {
    .error(
      "memberwise initializer declaration may only appear in product type declaration",
      range: range)
  }

  static func illegalParameterConvention(
    _ convention: PassingConvention,
    at range: SourceRange?
  ) -> Diagnostic {
    .error("'\(convention)' may only be used on parameters", range: range)
  }

  static func implicitReferenceToForeignReceiver(at range: SourceRange?) -> Diagnostic {
    .error("implicit reference to foreign receiver", range: range)
  }

  static func incompatibleLabels(
    found: [String?],
    expected: [String?],
    at range: SourceRange?
  ) -> Diagnostic {
    let ls = found.reduce(into: "", { (string, label) in string += (label ?? "_") + ":" })
    let rs = expected.reduce(into: "", { (string, label) in string += (label ?? "_") + ":" })
    return .error("incompatible labels: found '(\(ls))', expected '(\(rs))'", range: range)
  }

  static func incompatibleParameterCount(at range: SourceRange?) -> Diagnostic {
    .error("incompatible number of parameters", range: range)
  }

  static func incompatibleTupleLengths(at range: SourceRange?) -> Diagnostic {
    .error("tuples have different lengths", range: range)
  }

  static func incompatibleTypes(_ l: Type, _ r: Type, at range: SourceRange?) -> Diagnostic {
    .error("incompatible types '\(l)' and '\(r)'", range: range)
  }

  static func invalidAssociatedTypeExpr(_ name: String, at range: SourceRange?) -> Diagnostic {
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
    at range: SourceRange?
  ) -> Diagnostic {
    .error(
      "contextual closure type requires \(expected) argument(s), found \(found)",
      range: range)
  }

  static func invalidInoutBundleReturnType(expected: Type, at range: SourceRange?) -> Diagnostic {
    .error("inout-capable method bundle must return '\(expected)'", range: range)
  }

  static func invalidDestructuring(ofType type: Type, at range: SourceRange?) -> Diagnostic {
    .error("invalid destructuring of type '\(type)'", range: range)
  }

  static func invalidSelfTypeExpr(at range: SourceRange?) -> Diagnostic {
    .error("reference to 'Self' outside of a type context", range: range)
  }

  static func invalidParameterType(_ type: Type, at range: SourceRange?) -> Diagnostic {
    .error("invalid parameter type '\(type)'", range: range)
  }

  static func memberDeclHasCaptures(at range: SourceRange?) -> Diagnostic {
    .error("member declaration has captures", range: range)
  }

  static func missingReturnValue(at range: SourceRange?) -> Diagnostic {
    .error("non-void function should return a value", range: range)
  }

  static func missingTypeAnnotation(at range: SourceRange?) -> Diagnostic {
    .error("missing type annotation", range: range)
  }

  static func nestedOperatorDeclaration(at range: SourceRange?) -> Diagnostic {
    .error("operator declaration can only appear at top-level", range: range)
  }

  static func nonTraitType(_ type: Type, at range: SourceRange?) -> Diagnostic {
    .error("type '\(type)' is not a trait", range: range)
  }

  static func noConformance(
    of type: Type,
    to trait: TraitType,
    at range: SourceRange?,
    children: [Diagnostic] = []
  ) -> Diagnostic {
    .error("type '\(type)' does not conform to trait '\(trait)'", range: range, children: children)
  }

  static func noSkolemInConformance(_ type: Type, at range: SourceRange?) -> Diagnostic {
    .error(
      """
      type '\(type)' in conformance constraint does not refers to a generic parameter or \
      associated type
      """,
      range: range)
  }

  static func noSkolemInEquality(l: Type, r: Type, at range: SourceRange?) -> Diagnostic {
    .error(
      """
      neither type in equality constraint ('\(l)' or '\(r)') refers to a generic parameter or \
      associated type
      """,
      range: range)
  }

  static func notEnoughContextToInferArguments(at range: SourceRange?) -> Diagnostic {
    .error(
      "not enough contextual information to infer the arguments to generic parameters",
      range: range)
  }

  static func notSubtype(_ l: Type, of r: Type, at range: SourceRange?) -> Diagnostic {
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

  static func requires(method name: Name, withType type: Type) -> Diagnostic {
    .error("trait requires method '\(name)' with type '\(type)'")
  }

  static func staleConstraint(constraint: Constraint, at range: SourceRange?) -> Diagnostic {
    .error("stale constraint '\(constraint)'", range: range)
  }

  static func staticMemberUsedOnInstance(
    member: Name,
    type: Type,
    at range: SourceRange?
  ) -> Diagnostic {
    .error(
      "static member '\(member)' cannot be used on instance of type '\(type)'",
      range: range)
  }

  static func undefined(name: String, at range: SourceRange?) -> Diagnostic {
    .error("undefined name '\(name)' in this scope", range: range)
  }

  static func undefinedOperator(_ name: String, at range: SourceRange?) -> Diagnostic {
    .error("undefined operator '\(name)'", range: range)
  }

  static func unusedResult(ofType type: Type, at range: SourceRange?) -> Diagnostic {
    .warning("unused result of type '\(type)'", range: range)
  }

}
