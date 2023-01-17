import Core

extension Diagnostic {

  static func error(
    ambiguousDisjunctionAt range: SourceRange?
  ) -> Diagnostic {
    .error("ambiguous disjunction", range: range)
  }

  static func error(
    circularRefinementAt range: SourceRange?
  ) -> Diagnostic {
    .error("circular trait refinement", range: range)
  }

  static func error(
    circularDependencyAt range: SourceRange?
  ) -> Diagnostic {
    .error("circular dependency", range: range)
  }

  static func error(
    cannotConstructTrait trait: TraitType, at range: SourceRange?
  ) -> Diagnostic {
    .error(
      "cannot construct an instance of trait '\(trait)'; did you mean 'any \(trait)'?", range: range
    )
  }

  static func error(
    cannotInferComplexReturnTypeAt range: SourceRange?
  ) -> Diagnostic {
    .error(
      "cannot infer complex return type; add an explicit return type annotation", range: range)
  }

  static func error(
    conformanceToNonTraitType type: AnyType, at range: SourceRange?
  ) -> Diagnostic {
    .error("conformance to non-trait type '\(type)'", range: range)
  }

  static func error(
    declarationRequiresBodyAt range: SourceRange?
  ) -> Diagnostic {
    .error("declaration requires a body", range: range)
  }

  static func error(
    duplicateCaptureNamed name: String, at range: SourceRange?
  ) -> Diagnostic {
    .error("duplicate capture name '\(name)'", range: range)
  }

  static func error(
    duplicateOperatorNamed name: String, at range: SourceRange?
  ) -> Diagnostic {
    .error("duplicate operator declaration '\(name)'", range: range)
  }

  static func diganose(
    duplicateParameterNamed name: String, at range: SourceRange?
  ) -> Diagnostic {
    .error("duplicate parameter name '\(name)'", range: range)
  }

  static func error(
    nameRefersToValue expr: NodeID<NameExpr>, in ast: AST
  ) -> Diagnostic {
    .error(
      "expected type but '\(ast[expr].name.value)' refers to a value", range: ast[expr].origin)
  }

  static func error(
    genericDeclHasCapturesAt range: SourceRange?
  ) -> Diagnostic {
    .error("generic declaration has captures", range: range)
  }

  static func error(
    illegalMemberwiseInitAt range: SourceRange?
  ) -> Diagnostic {
    .error(
      "memberwise initializer declaration may only appear in product type declaration", range: range
    )
  }

  static func error(
    illegalParameterConvention convention: AccessEffect, at range: SourceRange?
  ) -> Diagnostic {
    .error("'\(convention)' may only be used on parameters", range: range)
  }

  static func error(
    labels found: [String?], incompatibleWith expected: [String?], at range: SourceRange?
  ) -> Diagnostic {
    .error(
      """
      incompatible labels: found '(\(Name.describe(labels: found)))', \
      expected '(\(Name.describe(labels: expected)))'
      """, range: range)
  }

  static func error(
    incompatibleParameterCountAt range: SourceRange?
  ) -> Diagnostic {
    .error("incompatible number of parameters", range: range)
  }

  static func error(
    incompatibleTupleLengthsAt range: SourceRange?
  ) -> Diagnostic {
    .error("tuples have different lengths", range: range)
  }

  static func error(
    type l: AnyType, incompatibleWith r: AnyType, at range: SourceRange?
  ) -> Diagnostic {
    .error("incompatible types '\(l)' and '\(r)'", range: range)
  }

  static func error(
    invalidUseOfAssociatedType name: String, at range: SourceRange?
  ) -> Diagnostic {
    .error(
      "associated type '\(name)' can only be used with a concrete type or generic type parameter",
      range: range)
  }

  static func error(
    expectedLambdaParameterCount: Int, found: Int, at range: SourceRange?
  ) -> Diagnostic {
    .error(
      """
      contextual lambda type requires \(expectedLambdaParameterCount) argument(s), found \(found)
      """, range: range)
  }

  static func error(
    inoutCapableMethodBundleMustReturn expectedReturnType: AnyType, at range: SourceRange?
  ) -> Diagnostic {
    .error("inout-capable method bundle must return '\(expectedReturnType)'", range: range)
  }

  static func error(
    invalidDestructuringOfType type: AnyType, at range: SourceRange?
  ) -> Diagnostic {
    .error("invalid destructuring of type '\(type)'", range: range)
  }

  static func error(
    invalidReferenceToSelfTypeAt range: SourceRange?
  ) -> Diagnostic {
    .error("reference to 'Self' outside of a type context", range: range)
  }

  static func error(
    invalidParameterType type: AnyType, at range: SourceRange?
  ) -> Diagnostic {
    .error("invalid parameter type '\(type)'", range: range)
  }

  static func error(
    missingReturnValueAt range: SourceRange?
  ) -> Diagnostic {
    .error("non-void function should return a value", range: range)
  }

  static func error(
    notATrait type: AnyType, at range: SourceRange?
  ) -> Diagnostic {
    .error("type '\(type)' is not a trait", range: range)
  }

  static func error(
    _ type: AnyType, doesNotConformTo trait: TraitType, at range: SourceRange?,
    because children: [Diagnostic] = []
  ) -> Diagnostic {
    .error(
      "type '\(type)' does not conform to trait '\(trait)'", range: range, children: children)
  }

  static func error(
    invalidConformanceConstraintTo type: AnyType, at range: SourceRange?
  ) -> Diagnostic {
    .error(
      """
      type '\(type)' in conformance constraint does not refers to a generic parameter or \
      associated type
      """, range: range)
  }

  static func error(
    invalidEqualityConstraintBetween l: AnyType, and r: AnyType, at range: SourceRange?
  ) -> Diagnostic {
    .error(
      """
      neither type in equality constraint ('\(l)' or '\(r)') refers to a generic parameter or \
      associated type
      """, range: range)
  }

  static func error(
    notEnoughContextToInferArgumentsAt range: SourceRange?
  ) -> Diagnostic {
    .error(
      "not enough contextual information to infer the arguments to generic parameters", range: range
    )
  }

  static func error(
    notEnoughContextToResolveMember name: Name, at range: SourceRange?
  ) -> Diagnostic {
    .error("not enough contextual information to resolve member '\(name)'", range: range)
  }

  static func error(
    noType name: Name, in domain: AnyType? = nil, at range: SourceRange?
  ) -> Diagnostic {
    if let domain = domain {
      return .error("type '\(domain)' has no type member '\(name.stem)'", range: range)
    } else {
      return .error("no type named '\(name.stem)' in this scope", range: range)
    }
  }

  static func error(
    traitRequiresMethod name: Name, withType type: AnyType
  ) -> Diagnostic {
    .error("trait requires method '\(name)' with type '\(type)'")
  }

  static func error(
    staleConstraint c: any Constraint
  ) -> Diagnostic {
    .error("stale constraint '\(c)'")
  }

  static func error(
    illegalUseOfStaticMember name: Name, onInstanceOf: AnyType, at range: SourceRange?
  ) -> Diagnostic {
    .error(
      "static member '\(name)' cannot be used on instance of '\(onInstanceOf)'", range: range)
  }

  static func error(
    undefinedName: String, at range: SourceRange?
  ) -> Diagnostic {
    .error("undefined name '\(undefinedName)' in this scope", range: range)
  }

  static func error(
    undefinedOperator name: String, at range: SourceRange?
  ) -> Diagnostic {
    .error("undefined operator '\(name)'", range: range)
  }

  static func warning(
    unusedResultOfType type: AnyType, at range: SourceRange?
  ) -> Diagnostic {
    .warning("unused result of type '\(type)'", range: range)
  }

  static func error(
    invalidGenericArgumentCountTo entity: SourceRepresentable<Name>, found: Int, expected: Int
  ) -> Diagnostic {
    if expected == 0 {
      return .error(
        "non-generic entity '\(entity.value)' has no generic parameters", range: entity.origin)
    } else {
      return .error(
        """
        too many generic arguments to entity '\(entity.value)' \
        (found \(found), expected \(expected)
        """, range: entity.origin)
    }
  }

  static func error(
    invalidGenericArgumentsTo entity: SourceRepresentable<Name>,
    candidateDiagnostics children: [Diagnostic]
  ) -> Diagnostic {
    .error(
      "invalid generic argument(s) for '\(entity.value)'", range: entity.origin, children: children)
  }

  static func error(
    argumentToNonGenericType type: AnyType, at range: SourceRange?
  ) -> Diagnostic {
    .error("non-generic type '\(type)' has no generic parameters", range: range)
  }

  static func error(
    metatypeRequiresOneArgumentAt range: SourceRange?
  ) -> Diagnostic {
    .error("reference to 'Metatype' requires exacly one static argument", range: range)
  }

  static func error(
    tooManyAnnotationsOnGenericValueParametersAt range: SourceRange?
  ) -> Diagnostic {
    .error("only one annotation is allowed on generic value parameter declarations", range: range)
  }

  static func error(
    invalidBufferTypeExprArgumentCount expr: NodeID<SubscriptCallExpr>, in ast: AST
  ) -> Diagnostic {
    .error(
      "buffer type expression requires exactly one argument", range: ast[ast[expr].callee].origin)
  }

  static func error(
    nonCallableType type: AnyType, at origin: SourceRange?
  ) -> Diagnostic {
    .error("cannot call value of non-callable type '\(type)'", range: origin)
  }

  static func error(
    noUnnamedSubscriptsIn domain: AnyType, at origin: SourceRange?
  ) -> Diagnostic {
    .error("type '\(domain)' has no unnamed subscripts", range: origin)
  }

  static func error(
    undefinedName name: Name, in domain: AnyType? = nil, at range: SourceRange?
  ) -> Diagnostic {
    if let domain = domain {
      return .error("type '\(domain)' has no member '\(name)'", range: range)
    } else {
      return .error("undefined name '\(name.stem)' in this scope", range: range)
    }
  }

  static func warning(
    sumTypeWithZeroElementsAt origin: SourceRange?
  ) -> Diagnostic {
    .warning("empty sum type is better expressed as 'Never'", range: origin)
  }

  static func error(
    sumTypeWithOneElementAt origin: SourceRange?
  ) -> Diagnostic {
    .error("sum types should contain at least 2 elements", range: origin)
  }

  static func error(
    valueInSumTypeAt origin: SourceRange?
  ) -> Diagnostic {
    .error("sum types cannot contain values", range: origin)
  }

  static func error(
    _ l: AnyType, isNotSubtypeOf r: AnyType, at origin: SourceRange?
  ) -> Diagnostic {
    .error("'\(l)' is not subtype of '\(r)'", range: origin)
  }

  static func error(
    cannotInitialize storageType: AnyType, with valueType: AnyType, at origin: SourceRange?
  ) -> Diagnostic {
    .error(
      "cannot initialize object of type '\(storageType)' with value of type '\(valueType)'",
      range: origin)
  }

  static func error(
    _ valueType: AnyType, doesNotMatchPatternAt origin: SourceRange?
  ) -> Diagnostic {
    .error(
      "value of type '\(valueType)' does not match binding pattern", range: origin)
  }

  static func error(
    _ subtype: AnyType, isNotStrictSubtypeOf supertype: AnyType, at origin: SourceRange?
  ) -> Diagnostic {
    .error(
      "type '\(subtype)' is not strict subtype of '\(supertype)'", range: origin)
  }

  static func error(
    ambiguousUse expr: NodeID<NameExpr>, in ast: AST, candidates: [AnyDeclID] = []
  ) -> Diagnostic {
    let children = candidates.compactMap({ (decl) -> Diagnostic? in
      if let origin = ast[decl].origin {
        return .error("candidate here", range: origin)
      } else {
        return nil
      }
    })

    return .error(
      "ambiguous use of '\(ast[expr].name.value)'", range: ast[expr].origin, children: children)
  }

}
