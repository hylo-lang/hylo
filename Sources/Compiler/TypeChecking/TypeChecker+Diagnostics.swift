extension Diagnostic {

  static func ambiguousTypeReference(name: String, range: SourceRange?) -> Diagnostic {
    Diagnostic(
      level: .error,
      message: "ambiguous reference to type named \(name)",
      location: range?.first(),
      window: range.map({ r in Diagnostic.Window(range: r) }))
  }

  static func circularRefinement(range: SourceRange?) -> Diagnostic {
    Diagnostic(
      level: .error,
      message: "circular trait refinment",
      location: range?.first(),
      window: range.map({ r in Diagnostic.Window(range: r) }))
  }

  static func circularDependency(range: SourceRange?) -> Diagnostic {
    Diagnostic(
      level: .error,
      message: "circular dependency",
      location: range?.first(),
      window: range.map({ r in Diagnostic.Window(range: r) }))
  }

  static func conformanceToNonTraitType(_ type: Type, range: SourceRange?) -> Diagnostic {
    Diagnostic(
      level: .error,
      message: "conformance to non-trait type '\(type)'",
      location: range?.first(),
      window: range.map({ r in Diagnostic.Window(range: r) }))
  }

  static func duplicateParameterName(_ name: String, range: SourceRange?) -> Diagnostic {
    Diagnostic(
      level: .error,
      message: "duplicate parameter name '\(name)'",
      location: range?.first(),
      window: range.map({ r in Diagnostic.Window(range: r) }))
  }

  static func illegalParameterConvention(
    _ convention: ParamConvention,
    range: SourceRange?
  ) -> Diagnostic {
    Diagnostic(
      level: .error,
      message: "'\(convention)' may only be used on parameters",
      location: range?.first(),
      window: range.map({ r in Diagnostic.Window(range: r) }))
  }

  static func incompatibleLabels(
    found: [String?],
    expected: [String?],
    range: SourceRange?
  ) -> Diagnostic {
    let ls = found.map({ "\($0 ?? "_")" }).joined(separator: ":")
    let rs = expected.map({ "\($0 ?? "_")" }).joined(separator: ":")

    return Diagnostic(
      level: .error,
      message: "incompatible labels: found '(\(ls))', expected '(\(rs))'",
      location: range?.first(),
      window: range.map({ r in Diagnostic.Window(range: r) }))
  }

  static func invalidAssociatedTypeExpr(_ name: String, range: SourceRange?) -> Diagnostic {
    Diagnostic(
      level: .error,
      message: """
        associated type '\(name)' can only be used referred to as a member of a generic type \
        parameter, a conformance lens, or another associated type
        """,
      location: range?.first(),
      window: range.map({ r in Diagnostic.Window(range: r) }))
  }

  static func invalidDestructuring(ofType type: Type, range: SourceRange?) -> Diagnostic {
    Diagnostic(
      level: .error,
      message: "invalid destructuring of type '\(type)'",
      location: range?.first(),
      window: range.map({ r in Diagnostic.Window(range: r) }))
  }

  static func invalidSelfTypeExpr(range: SourceRange?) -> Diagnostic {
    Diagnostic(
      level: .error,
      message: "reference to 'Self' outside of a type context",
      location: range?.first(),
      window: range.map({ r in Diagnostic.Window(range: r) }))
  }

  static func missingTypeAnnotation(range: SourceRange?) -> Diagnostic {
    Diagnostic(
      level: .error,
      message: "missing type annotation",
      location: range?.first(),
      window: range.map({ r in Diagnostic.Window(range: r) }))
  }

  static func nonTraitType(_ type: Type, range: SourceRange?) -> Diagnostic {
    Diagnostic(
      level: .error,
      message: "type '\(type)' is not a trait",
      location: range?.first(),
      window: range.map({ r in Diagnostic.Window(range: r) }))
  }

  static func noConformance(
    of type: Type,
    to trait: TraitType,
    range: SourceRange?
  ) -> Diagnostic {
    Diagnostic(
      level: .error,
      message: "type '\(type)' does not conform to trait '\(trait)'",
      location: range?.first(),
      window: range.map({ r in Diagnostic.Window(range: r) }))
  }

  static func noSkolemInConformance(_ type: Type, range: SourceRange?) -> Diagnostic {
    Diagnostic(
      level: .error,
      message: """
        type '\(type)' in conformance constraint does not refers to a generic parameter or \
        associated type
        """,
      location: range?.first(),
      window: range.map({ r in Diagnostic.Window(range: r) }))
  }

  static func noSkolemInEquality(l: Type, r: Type, range: SourceRange?) -> Diagnostic {
    Diagnostic(
      level: .error,
      message: """
        neither type in equality constraint ('\(l)' or '\(r)') refers to a generic parameter or \
        associated type
        """,
      location: range?.first(),
      window: range.map({ r in Diagnostic.Window(range: r) }))
  }

  static func notEnoughContextToInferArguments(range: SourceRange?) -> Diagnostic {
    Diagnostic(
      level: .error,
      message: "not enough contextual information to infer the arguments to generic parameters",
      location: range?.first(),
      window: range.map({ r in Diagnostic.Window(range: r) }))
  }

  static func noType(
    named name: String,
    in domain: Type? = nil,
    range: SourceRange?
  ) -> Diagnostic {
    let message: String
    if let domain = domain {
      message = "type '\(domain)' has no type member '\(name)'"
    } else {
      message = "no type named '\(name)' in this scope"
    }

    return Diagnostic(
      level: .error,
      message: message,
      location: range?.first(),
      window: range.map({ r in Diagnostic.Window(range: r) }))
  }

  static func staleConstraint(constraint: Constraint, range: SourceRange?) -> Diagnostic {
    Diagnostic(
      level: .error,
      message: "stale constraint '\(constraint)'",
      location: range?.first(),
      window: range.map({ r in Diagnostic.Window(range: r) }))
  }

  static func undefined(name: String, range: SourceRange?) -> Diagnostic {
    Diagnostic(
      level: .error,
      message: "undefined name '\(name)' in this scope",
      location: range?.first(),
      window: range.map({ r in Diagnostic.Window(range: r) }))
  }

}
