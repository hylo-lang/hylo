/// A collection of module declarations representing an abstract syntax tree.
public struct AST {

  /// The declarations in the AST.
  private var decls: [Decl]

  /// A collection with the indices of the modules of the AST.
  public private(set) var modules: [DeclIndex<ModuleDecl>]

  /// Creates an empty AST.
  public init() {
    decls = []
    modules = []
  }

  /// The index of the module containing Val's standard library, if present in the AST.
  public var std: DeclIndex<ModuleDecl>?

  /// Returns the scope hierarchy of the AST.
  func scopeHierarchy() -> ScopeHierarchy {
    var builder = ScopeHierarchyBuilder()
    return builder.build(hierarchyOf: self)
  }

  /// Appends a declaration to the AST.
  public mutating func append<T: Decl>(decl: T) -> DeclIndex<T> {
    let i = DeclIndex<T>(index: decls.count)
    decls.append(decl)
    if decl is ModuleDecl { modules.append(i as! DeclIndex<ModuleDecl>) }
    return i
  }

  /// Accesses the declaration at `position`.
  public subscript<T>(position: DeclIndex<T>) -> T {
    _read { yield decls[position.index] as! T }
    _modify {
      var d = decls[position.index] as! T
      defer { decls[position.index] = d }
      yield &d
    }
  }

  /// Accesses the declaration at `position`.
  public subscript(position: AnyDeclIndex) -> Decl {
    _read { yield decls[position.index] }
  }

}
