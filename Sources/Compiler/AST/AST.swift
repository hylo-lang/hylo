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

/// The index of a declaration in an AST.
public struct DeclIndex<T: Decl>: Hashable {

  fileprivate var index: Int

  /// Returns this index type-erased.
  public func erased() -> AnyDeclIndex { AnyDeclIndex(self) }

}

/// The type-erased index of a declaration in an AST.
public struct AnyDeclIndex: Hashable {

  fileprivate var index: Int

  /// Creates a type-erased index from a typed index.
  public init<T>(_ other: DeclIndex<T>) {
    index = other.index
  }

}
