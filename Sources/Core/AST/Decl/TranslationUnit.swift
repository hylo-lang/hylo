/// A collection of declarations at the top-level of a source file.
public struct TranslationUnit: Node, LexicalScope {

  public let site: SourceRange

  /// The declarations in this unit.
  public private(set) var decls: [AnyDeclID]

  /// Creates an instance with the given properties.
  public init(decls: [AnyDeclID], site: SourceRange) {
    self.decls = decls
    self.site = site
  }

  public func validateForm(in ast: AST, into diagnostics: inout DiagnosticSet) {
    for d in decls {
      ast.validateGlobalScopeMember(d, atTopLevel: true, reportingDiagnosticsTo: &diagnostics)
    }
  }

}
