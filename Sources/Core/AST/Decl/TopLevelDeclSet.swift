/// A set of declarations at the top-level of a source file.
public struct TopLevelDeclSet: Node, LexicalScope {

  /// The declarations in the set.
  public private(set) var decls: [AnyDeclID]

  /// Creates an instance with the given properties.
  public init(decls: [AnyDeclID] = []) {
    self.decls = decls
  }

  public var origin: SourceRange? { nil }

  public func validateForm(in ast: AST, into diagnostics: inout Diagnostics) {
    for d in decls {
      ast.validateGlobalScopeMember(d, into: &diagnostics, atTopLevel: true)
    }
  }

}
