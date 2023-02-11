import Core

extension Diagnostic {

  static func error(ambiguousDisjunctionAt site: SourceRange) -> Diagnostic {
    .error("ambiguous disjunction", at: site)
  }

  static func error(circularRefinementAt site: SourceRange) -> Diagnostic {
    .error("circular trait refinement", at: site)
  }

  static func error(circularDependencyAt site: SourceRange) -> Diagnostic {
    .error("circular dependency", at: site)
  }

  static func error(cannotConstructTrait trait: TraitType, at site: SourceRange) -> Diagnostic {
    .error(
      "cannot construct an instance of trait '\(trait)'; did you mean 'any \(trait)'?", at: site)
  }

  static func error(cannotInferComplexReturnTypeAt site: SourceRange) -> Diagnostic {
    .error("cannot infer complex return type; add an explicit return type annotation", at: site)
  }

  static func error(conformanceToNonTraitType type: AnyType, at site: SourceRange) -> Diagnostic {
    .error("conformance to non-trait type '\(type)'", at: site)
  }

  static func error(
    redundantConformance c: Conformance,
    at site: SourceRange,
    alreadyDeclaredAt originalSite: SourceRange
  ) -> Diagnostic {
    .error(
      "redundant conformance of '\(c.model)' to trait '\(c.concept)'", at: site,
      notes: [
        .error("conformance already declared here", at: originalSite)
      ])
  }

  static func error(declarationRequiresBodyAt site: SourceRange) -> Diagnostic {
    .error("declaration requires a body", at: site)
  }

  static func error(duplicateCaptureNamed name: String, at site: SourceRange) -> Diagnostic {
    .error("duplicate capture name '\(name)'", at: site)
  }

  static func error(duplicateOperatorNamed name: String, at site: SourceRange) -> Diagnostic {
    .error("duplicate operator declaration '\(name)'", at: site)
  }

  static func diganose(duplicateParameterNamed name: String, at site: SourceRange) -> Diagnostic {
    .error("duplicate parameter name '\(name)'", at: site)
  }

  static func error(nameRefersToValue expr: NodeID<NameExpr>, in ast: AST) -> Diagnostic {
    .error("expected type but '\(ast[expr].name.value)' refers to a value", at: ast[expr].site)
  }

  static func error(genericDeclHasCapturesAt site: SourceRange) -> Diagnostic {
    .error("generic declaration has captures", at: site)
  }

  static func error(illegalMemberwiseInitAt site: SourceRange) -> Diagnostic {
    .error(
      "memberwise initializer declaration may only appear in product type declaration", at: site)
  }

  static func error(illegalParameterConvention convention: AccessEffect, at site: SourceRange)
    -> Diagnostic
  {
    .error("'\(convention)' may only be used on parameters", at: site)
  }

  static func error(
    labels found: [String?], incompatibleWith expected: [String?], at site: SourceRange
  ) -> Diagnostic {
    .error(
      """
      incompatible labels: found '(\(Name.describe(labels: found)))', \
      expected '(\(Name.describe(labels: expected)))'
      """, at: site)
  }

  static func error(incompatibleParameterCountAt site: SourceRange) -> Diagnostic {
    .error("incompatible number of parameters", at: site)
  }

  static func error(incompatibleTupleLengthsAt site: SourceRange) -> Diagnostic {
    .error("tuples have different lengths", at: site)
  }

  static func error(type l: AnyType, incompatibleWith r: AnyType, at site: SourceRange)
    -> Diagnostic
  {
    .error("incompatible types '\(l)' and '\(r)'", at: site)
  }

  static func error(invalidUseOfAssociatedType name: String, at site: SourceRange) -> Diagnostic {
    .error(
      "associated type '\(name)' can only be used with a concrete type or generic type parameter",
      at: site)
  }

  static func error(expectedLambdaParameterCount: Int, found: Int, at site: SourceRange)
    -> Diagnostic
  {
    .error(
      """
      contextual lambda type requires \(expectedLambdaParameterCount) argument(s), found \(found)
      """, at: site)
  }

  static func error(
    inoutCapableMethodBundleMustReturn expectedReturnType: AnyType, at site: SourceRange
  ) -> Diagnostic {
    .error("inout-capable method bundle must return '\(expectedReturnType)'", at: site)
  }

  static func error(invalidDestructuringOfType type: AnyType, at site: SourceRange) -> Diagnostic {
    .error("invalid destructuring of type '\(type)'", at: site)
  }

  static func error(invalidReferenceToSelfTypeAt site: SourceRange) -> Diagnostic {
    .error("reference to 'Self' outside of a type context", at: site)
  }

  static func error(invalidParameterType type: AnyType, at site: SourceRange) -> Diagnostic {
    .error("invalid parameter type '\(type)'", at: site)
  }

  static func error(missingReturnValueAt site: SourceRange) -> Diagnostic {
    .error("non-void function should return a value", at: site)
  }

  static func error(notATrait type: AnyType, at site: SourceRange) -> Diagnostic {
    .error("type '\(type)' is not a trait", at: site)
  }

  static func error(
    _ type: AnyType, doesNotConformTo trait: TraitType, at site: SourceRange,
    because notes: [Diagnostic] = []
  ) -> Diagnostic {
    .error("type '\(type)' does not conform to trait '\(trait)'", at: site, notes: notes)
  }

  static func error(invalidConformanceConstraintTo type: AnyType, at site: SourceRange)
    -> Diagnostic
  {
    .error(
      """
      type '\(type)' in conformance constraint does not refers to a generic parameter or \
      associated type
      """, at: site)
  }

  static func error(
    invalidEqualityConstraintBetween l: AnyType, and r: AnyType, at site: SourceRange
  ) -> Diagnostic {
    .error(
      """
      neither type in equality constraint ('\(l)' or '\(r)') refers to a generic parameter or \
      associated type
      """, at: site)
  }

  static func error(notEnoughContextToInferArgumentsAt site: SourceRange) -> Diagnostic {
    .error(
      "not enough contextual information to infer the arguments to generic parameters", at: site)
  }

  static func error(notEnoughContextToResolveMember name: Name, at site: SourceRange) -> Diagnostic
  {
    .error("not enough contextual information to resolve member '\(name)'", at: site)
  }

  static func error(noType name: Name, in domain: AnyType? = nil, at site: SourceRange)
    -> Diagnostic
  {
    if let domain = domain {
      return .error("type '\(domain)' has no type member '\(name.stem)'", at: site)
    } else {
      return .error("no type named '\(name.stem)' in this scope", at: site)
    }
  }

  static func error(
    traitRequiresMethod name: Name, withType type: AnyType, at site: SourceRange
  ) -> Diagnostic {
    .error("trait requires method '\(name)' with type '\(type)'", at: site)
  }

  static func error(staleConstraint c: any Constraint) -> Diagnostic {
    .error("stale constraint '\(c)'", at: c.cause.site)
  }

  static func error(
    illegalUseOfStaticMember name: Name, onInstanceOf: AnyType, at site: SourceRange
  ) -> Diagnostic {
    .error("static member '\(name)' cannot be used on instance of '\(onInstanceOf)'", at: site)
  }

  static func error(undefinedName: String, at site: SourceRange) -> Diagnostic {
    .error("undefined name '\(undefinedName)' in this scope", at: site)
  }

  static func error(undefinedOperator name: String, at site: SourceRange) -> Diagnostic {
    .error("undefined operator '\(name)'", at: site)
  }

  static func warning(unusedResultOfType type: AnyType, at site: SourceRange) -> Diagnostic {
    .warning("unused result of type '\(type)'", at: site)
  }

  static func error(
    invalidGenericArgumentCountTo entity: SourceRepresentable<Name>, found: Int, expected: Int
  ) -> Diagnostic {
    if expected == 0 {
      return .error(
        "non-generic entity '\(entity.value)' has no generic parameters", at: entity.site)
    } else {
      return .error(
        """
        too many generic arguments to entity '\(entity.value)' \
        (found \(found), expected \(expected)
        """, at: entity.site)
    }
  }

  static func error(
    invalidGenericArgumentsTo entity: SourceRepresentable<Name>,
    candidateDiagnostics notes: [Diagnostic]
  ) -> Diagnostic {
    .error("invalid generic argument(s) for '\(entity.value)'", at: entity.site, notes: notes)
  }

  static func error(argumentToNonGenericType type: AnyType, at site: SourceRange) -> Diagnostic {
    .error("non-generic type '\(type)' has no generic parameters", at: site)
  }

  static func error(metatypeRequiresOneArgumentAt site: SourceRange) -> Diagnostic {
    .error("reference to 'Metatype' requires exacly one static argument", at: site)
  }

  static func error(tooManyAnnotationsOnGenericValueParametersAt site: SourceRange) -> Diagnostic {
    .error("only one annotation is allowed on generic value parameter declarations", at: site)
  }

  static func error(invalidBufferTypeExprArgumentCount expr: NodeID<SubscriptCallExpr>, in ast: AST)
    -> Diagnostic
  {
    .error("buffer type expression requires exactly one argument", at: ast[ast[expr].callee].site)
  }

  static func error(nonCallableType type: AnyType, at site: SourceRange) -> Diagnostic {
    .error("cannot call value of non-callable type '\(type)'", at: site)
  }

  static func error(noUnnamedSubscriptsIn domain: AnyType, at site: SourceRange) -> Diagnostic {
    .error("type '\(domain)' has no unnamed subscripts", at: site)
  }

  static func error(undefinedName name: Name, in domain: AnyType? = nil, at site: SourceRange)
    -> Diagnostic
  {
    if let domain = domain {
      return .error("type '\(domain)' has no member '\(name)'", at: site)
    } else {
      return .error("undefined name '\(name.stem)' in this scope", at: site)
    }
  }

  static func warning(sumTypeWithZeroElementsAt site: SourceRange) -> Diagnostic {
    .warning("empty sum type is better expressed as 'Never'", at: site)
  }

  static func error(sumTypeWithOneElementAt site: SourceRange) -> Diagnostic {
    .error("sum types should contain at least 2 elements", at: site)
  }

  static func error(valueInSumTypeAt site: SourceRange) -> Diagnostic {
    .error("sum types cannot contain values", at: site)
  }

  static func error(_ l: AnyType, isNotSubtypeOf r: AnyType, at site: SourceRange) -> Diagnostic {
    .error("'\(l)' is not subtype of '\(r)'", at: site)
  }

  static func error(
    cannotInitialize storageType: AnyType, with valueType: AnyType, at site: SourceRange
  ) -> Diagnostic {
    .error(
      "cannot initialize object of type '\(storageType)' with value of type '\(valueType)'",
      at: site)
  }

  static func error(_ valueType: AnyType, doesNotMatchPatternAt site: SourceRange) -> Diagnostic {
    .error("value of type '\(valueType)' does not match binding pattern", at: site)
  }

  static func error(
    _ subtype: AnyType, isNotStrictSubtypeOf supertype: AnyType, at site: SourceRange
  ) -> Diagnostic {
    .error("type '\(subtype)' is not strict subtype of '\(supertype)'", at: site)
  }

  static func error(ambiguousUse expr: NodeID<NameExpr>, in ast: AST, candidates: [AnyDeclID] = [])
    -> Diagnostic
  {
    let notes = candidates.map { Diagnostic.error("candidate here", at: ast[$0].site) }
    return .error("ambiguous use of '\(ast[expr].name.value)'", at: ast[expr].site, notes: notes)
  }

}
