extension DeclSpace {

  /// The type and value declarations directly enclosed in this space.
  ///
  /// This default implementation walks the subtree rooted by this node to collect all type and
  /// value declarations that do not belong to a nested space. Conforming types may override this
  /// behavior for more a optimized strategy.
  public var localTypeAndValueDecls: [TypeOrValueDecl] {
    let finder = LocalTypeAndValueDeclFinder()
    _ = accept(finder)
    return finder.matches
  }

  public func lookup(_ unqualifiedName: String, in context: Context) -> LookupResult {
    var space: DeclSpace? = self
    var matches: [TypeOrValueDecl] = []

    // Only function and type declarations are overloadable. Thus we must filter out every other
    // value declaration once the first has been found.
    var hasNonOverloadableDecl = false

    while let ns = space {
      // Enumerate the symbols declared directly within the current space.
      var locals = ns.localTypeAndValueDecls.filter({ $0.name == unqualifiedName })
      if hasNonOverloadableDecl {
        locals = locals.filter({ decl in decl.isOverloadable })
      } else {
        hasNonOverloadableDecl = locals.contains(where: { decl in !decl.isOverloadable })
      }

      matches.append(contentsOf: locals)
      space = ns.parentDeclSpace
    }

    // Handle the implicit import of the built-in module in the standard library.
    if context.isCompilingStdLib && (unqualifiedName == "Builtin") {
      return LookupResult(matches: [context.builtin])
    }

    return LookupResult(matches: matches)
  }

}

extension TypeDecl {

  public func lookup(qualifiedName name: String) -> LookupResult {
    // FIXME: Visit extensions.
    return LookupResult(matches: localTypeAndValueDecls.filter({ $0.name == name }))
  }

}

/// The result of a name lookup in a declaration space.
public struct LookupResult {

  public init(matches: [TypeOrValueDecl]) {
    self.matches = matches
  }

  public let matches: [TypeOrValueDecl]

  public var isEmpty: Bool {
    return matches.isEmpty
  }

  public var containsTypeDecls: Bool {
    return matches.contains(where: { $0 is TypeDecl })
  }

  public var containsValueDecls: Bool {
    return matches.contains(where: { $0 is ValueDecl })
  }

  public var typeDecls: [TypeDecl] {
    return matches.compactMap({ $0 as? TypeDecl })
  }

  public var valueDecls: [ValueDecl] {
    return matches.compactMap({ $0 as? ValueDecl })
  }

}

/// An AST walker that extracts the type and value declarations in a given decl space.
fileprivate final class LocalTypeAndValueDeclFinder: NodeWalker {

  var matches: [TypeOrValueDecl] = []

  override func willVisit(_ decl: Decl) -> (shouldWalk: Bool, nodeBefore: Decl) {
    if let d = decl as? TypeOrValueDecl {
      matches.append(d)
    }
    return (!(decl is DeclSpace), decl)
  }

  override func willVisit(_ stmt: Stmt) -> (shouldWalk: Bool, nodeBefore: Stmt) {
    return (!(stmt is DeclSpace), stmt)
  }

  override func willVisit(_ expr: Expr) -> (shouldWalk: Bool, nodeBefore: Expr) {
    return (!(expr is DeclSpace), expr)
  }

  override func willVisit(_ pattern: Pattern) -> (shouldWalk: Bool, nodeBefore: Pattern) {
    return (!(pattern is DeclSpace), pattern)
  }

  override func willVisit(_ repr: TypeRepr) -> (shouldWalk: Bool, nodeBefore: TypeRepr) {
    return (!(repr is DeclSpace), repr)
  }

}
