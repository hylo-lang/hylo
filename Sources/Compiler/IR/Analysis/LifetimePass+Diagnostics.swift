extension Diagnostic {

  static func unusedBinding(name: Identifier, range: SourceRange?) -> Diagnostic {
    Diagnostic(
      level: .warning,
      message: "binding '\(name)' was never used",
      location: range?.first(),
      window: range.map({ r in Diagnostic.Window(range: r) }))
  }

}
