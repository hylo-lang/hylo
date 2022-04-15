extension Diagnostic {

  static func ambiguousTypeReference(name: String, range: SourceRange?) -> Diagnostic {
    Diagnostic(
      level: .error,
      message: "ambiguous reference to type named \(name)",
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

  static func invalidSelfTypeExpr(range: SourceRange?) -> Diagnostic {
    Diagnostic(
      level: .error,
      message: "reference to 'Self' outside of a type context",
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

  static func noType(
    named name: String,
    in domain: Type? = nil,
    range: SourceRange?
  ) -> Diagnostic {
    let message: String
    if let domain = domain {
      message = "type '\(domain)' has no type member '\(name)'"
    } else {
      message = "no type named '\(name)' in scope"
    }

    return Diagnostic(
      level: .error,
      message: message,
      location: range?.first(),
      window: range.map({ r in Diagnostic.Window(range: r) }))
  }

}
