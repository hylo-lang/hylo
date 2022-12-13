extension Diagnostic {

  static func diagnose(
    ambiguousDisjunctionAt range: SourceRange?
  ) -> Diagnostic {
    .error("ambiguous disjunction", range: range)
  }

  static func diagnose(
    ambiguousUse expr: NodeID<NameExpr>,
    in ast: AST
  ) -> Diagnostic {
    .error("ambiguous use of '\(ast[expr].name.value)'", range: ast[expr].origin)
  }

  static func diagnose(
    circularRefinementAt range: SourceRange?
  ) -> Diagnostic {
    .error("circular trait refinement", range: range)
  }

  static func diagnose(
    circularDependencyAt range: SourceRange?
  ) -> Diagnostic {
    .error("circular dependency", range: range)
  }

  static func diagnose(
    cannotConstructTrait trait: TraitType,
    at range: SourceRange?
  ) -> Diagnostic {
    .error(
      "cannot construct an instance of trait '\(trait)'; did you mean 'any \(trait)'?",
      range: range)
  }

  static func diagnose(
    cannotInferComplexReturnTypeAt range: SourceRange?
  ) -> Diagnostic {
    .error(
      "cannot infer complex return type; add an explicit return type annotation",
      range: range)
  }

  static func diagnose(
    conformanceToNonTraitType type: AnyType,
    at range: SourceRange?
  ) -> Diagnostic {
    .error("conformance to non-trait type '\(type)'", range: range)
  }

  static func diagnose(
    declarationRequiresBodyAt range: SourceRange?
  ) -> Diagnostic {
    .error("declaration requires a body", range: range)
  }

  static func diagnose(
    duplicateCaptureNamed name: String,
    at range: SourceRange?
  ) -> Diagnostic {
    .error("duplicate capture name '\(name)'", range: range)
  }

  static func diagnose(
    duplicateOperatorNamed name: String,
    at range: SourceRange?
  ) -> Diagnostic {
    .error("duplicate operator declaration '\(name)'", range: range)
  }

  static func diganose(
    duplicateParameterNamed name: String,
    at range: SourceRange?
  ) -> Diagnostic {
    .error("duplicate parameter name '\(name)'", range: range)
  }

  static func diagnose(
    nameRefersToValue expr: NodeID<NameExpr>,
    in ast: AST
  ) -> Diagnostic {
    .error(
      "expected type but '\(ast[expr].name.value)' refers to a value",
      range: ast[expr].origin)
  }

  static func diagnose(
    genericDeclHasCapturesAt range: SourceRange?
  ) -> Diagnostic {
    .error("generic declaration has captures", range: range)
  }

  static func diagnose(
    illegalMemberwiseInitAt range: SourceRange?
  ) -> Diagnostic {
    .error(
      "memberwise initializer declaration may only appear in product type declaration",
      range: range)
  }

  static func diagnose(
    illegalParameterConvention convention: PassingConvention,
    at range: SourceRange?
  ) -> Diagnostic {
    .error("'\(convention)' may only be used on parameters", range: range)
  }

  static func diagnose(
    labels found: [String?],
    incompatibleWith expected: [String?],
    at range: SourceRange?
  ) -> Diagnostic {
    .error(
      """
      incompatible labels: found '(\(Name.describe(labels: found)))', \
      expected '(\(Name.describe(labels: expected)))'
      """,
      range: range)
  }

  static func diagnose(
    incompatibleParameterCountAt range: SourceRange?
  ) -> Diagnostic {
    .error("incompatible number of parameters", range: range)
  }

  static func diagnose(
    incompatibleTupleLengthsAt range: SourceRange?
  ) -> Diagnostic {
    .error("tuples have different lengths", range: range)
  }

  static func diagnose(
    type l: AnyType,
    incompatibleWith r: AnyType,
    at range: SourceRange?
  ) -> Diagnostic {
    .error("incompatible types '\(l)' and '\(r)'", range: range)
  }

  static func diagnose(
    invalidUseOfAssociatedType name: String,
    at range: SourceRange?
  ) -> Diagnostic {
    .error(
      "associated type '\(name)' can only be used with a concrete type or generic type parameter",
      range: range)
  }

  static func diagnose(
    expectedLambdaParameterCount: Int,
    found: Int,
    at range: SourceRange?
  ) -> Diagnostic {
    .error(
      """
      contextual lambda type requires \(expectedLambdaParameterCount) argument(s), found \(found)
      """,
      range: range)
  }

  static func diagnose(
    inoutCapableMethodBundleMustReturn expectedReturnType: AnyType,
    at range: SourceRange?
  ) -> Diagnostic {
    .error("inout-capable method bundle must return '\(expectedReturnType)'", range: range)
  }

  static func diagnose(
    invalidDestructuringOfType type: AnyType,
    at range: SourceRange?
  ) -> Diagnostic {
    .error("invalid destructuring of type '\(type)'", range: range)
  }

  static func diagnose(
    invalidReferenceToSelfTypeAt range: SourceRange?
  ) -> Diagnostic {
    .error("reference to 'Self' outside of a type context", range: range)
  }

  static func diagnose(
    invalidParameterType type: AnyType,
    at range: SourceRange?
  ) -> Diagnostic {
    .error("invalid parameter type '\(type)'", range: range)
  }

  static func diagnose(
    missingReturnValueAt range: SourceRange?
  ) -> Diagnostic {
    .error("non-void function should return a value", range: range)
  }

  static func diagnose(
    notATrait type: AnyType,
    at range: SourceRange?
  ) -> Diagnostic {
    .error("type '\(type)' is not a trait", range: range)
  }

  static func diagnose(
    _ type: AnyType,
    doesNotConformTo trait: TraitType,
    at range: SourceRange?,
    because children: [Diagnostic] = []
  ) -> Diagnostic {
    .error(
      "type '\(type)' does not conform to trait '\(trait)'",
      range: range,
      children: children)
  }

  static func diagnose(
    invalidConformanceConstraintTo type: AnyType,
    at range: SourceRange?
  ) -> Diagnostic {
    .error(
      """
      type '\(type)' in conformance constraint does not refers to a generic parameter or \
      associated type
      """,
      range: range)
  }

  static func diagnose(
    invalidEqualityConstraintBetween l: AnyType,
    and r: AnyType,
    at range: SourceRange?
  ) -> Diagnostic {
    .error(
      """
      neither type in equality constraint ('\(l)' or '\(r)') refers to a generic parameter or \
      associated type
      """,
      range: range)
  }

  static func diagnose(
    notEnoughContextToInferArgumentsAt range: SourceRange?
  ) -> Diagnostic {
    .error(
      "not enough contextual information to infer the arguments to generic parameters",
      range: range)
  }

  static func diagnose(
    notEnoughContextToResolveMember name: Name,
    at range: SourceRange?
  ) -> Diagnostic {
    .error("not enough contextual information to resolve member '\(name)'", range: range)
  }

  static func diagnose(
    type l: AnyType,
    isNotSubtypeOf r: AnyType,
    at range: SourceRange?
  ) -> Diagnostic {
    .error("'\(l)' is not subtype of '\(r)'", range: range)
  }

  static func diagnose(
    noType name: Name,
    in domain: AnyType? = nil,
    at range: SourceRange?
  ) -> Diagnostic {
    if let domain = domain {
      return .error("type '\(domain)' has no type member '\(name.stem)'", range: range)
    } else {
      return .error("no type named '\(name.stem)' in this scope", range: range)
    }
  }

  static func diagnose(
    traitRequiresMethod name: Name,
    withType type: AnyType
  ) -> Diagnostic {
    .error("trait requires method '\(name)' with type '\(type)'")
  }

  static func diagnose(
    staleConstraint c: any Constraint
  ) -> Diagnostic {
    .error("stale constraint '\(c)'")
  }

  static func diagnose(
    illegalUseOfStaticMember name: Name,
    onInstanceOf: AnyType,
    at range: SourceRange?
  ) -> Diagnostic {
    .error(
      "static member '\(name)' cannot be used on instance of '\(onInstanceOf)'",
      range: range)
  }

  static func diagnose(
    undefinedName: String,
    at range: SourceRange?
  ) -> Diagnostic {
    .error("undefined name '\(undefinedName)' in this scope", range: range)
  }

  static func diagnose(
    undefinedOperator name: String,
    at range: SourceRange?
  ) -> Diagnostic {
    .error("undefined operator '\(name)'", range: range)
  }

  static func diagnose(
    unusedResultOfType type: AnyType,
    at range: SourceRange?
  ) -> Diagnostic {
    .warning("unused result of type '\(type)'", range: range)
  }

  static func diagnose(
    argumentToNonGenericType type: AnyType,
    at range: SourceRange?
  ) -> Diagnostic {
    .error("non-generic type 'type' has no generic parameters", range: range)
  }

  static func diagnose(
    metatypeRequiresOneArgumentAt range: SourceRange?
  ) -> Diagnostic {
    .error("reference to 'Metatype' requires exacly one static argument", range: range)
  }

  static func diagnose(
    tooManyAnnotationsOnGenericValueParametersAt range: SourceRange?
  ) -> Diagnostic {
    .error("only one annotation is allowed on generic value parameter declarations", range: range)
  }

  static func diagnose(
    invalidBufferTypeExprArgumentCount expr: NodeID<SubscriptCallExpr>,
    in ast: AST
  ) -> Diagnostic {
    .error(
      "buffer type expression requires exactly one argument",
      range: ast[ast[expr].callee].origin)
  }

  static func diagnose(
    nonCallableType type: AnyType,
    at origin: SourceRange?
  ) -> Diagnostic {
    .error("cannot call value of non-callable type '\(type)'", range: origin)
  }

  static func diagnose(
    noUnnamedSubscriptsIn domain: AnyType,
    at origin: SourceRange?
  ) -> Diagnostic {
    return .error("type '\(domain)' has no unnamed subscripts", range: origin)
  }

}
