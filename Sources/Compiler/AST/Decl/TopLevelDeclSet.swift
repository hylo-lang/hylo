/// A set of declarations at the top-level of a source file.
public struct TopLevelDeclSet: Node, LexicalScope {

  public static let kind = NodeKind.topLevelDeclSet

  /// The declarations in the set.
  public private(set) var decls: [AnyDeclID]

  /// Creates an instance with the given properties.
  public init(decls: [AnyDeclID] = []) {
    self.decls = decls
  }

  public func checkInvariants(in ast: AST) -> FallibleWithDiagnostic<Void> {
    let ds: [Diagnostic] = decls.reduce(into: [], { (ds, member) in
      ds.append(contentsOf: ast.checkValidGlobalScopeMember(member, atTopLevel: true).diagnostics)
    })
    return ds.isEmpty ? .success(()) : .failure(DiagnosedError(ds))
  }

}
