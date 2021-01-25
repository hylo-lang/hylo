extension DeclSpace {

  /// The type and value declarations directly enclosed in this space.
  ///
  /// This default implementation walks the subtree rooted by this node to collect all type and
  /// value declarations that do not belong to a nested space. Conforming types may override this
  /// behavior for more a optimized strategy.
  public var localTypeAndValueDecls: (types: [TypeDecl], values: [ValueDecl]) {
    let finder = LocalTypeAndValueDeclFinder()
    _ = accept(finder)
    return (finder.types, finder.values)
  }

  public func lookup(unqualified name: String, in context: Context) -> LookupResult {
    var space: DeclSpace? = self
    var result = LookupResult()

    // Only function and type declarations are overloadable. Thus we must filter out every other
    // value declaration once the first has been found.
    var hasNonOverloadableDecl = false

    while let ns = space {
      // Enumerate the symbols declared directly within the current space.
      let (types, values) = ns.localTypeAndValueDecls
      var locals = LookupResult(
        types: types.filter({ $0.name == name }), values: values.filter({ $0.name == name }))
      if hasNonOverloadableDecl {
        locals = locals.filter({ $0.isOverloadable })
      } else {
        hasNonOverloadableDecl = locals.contains(where: { !$0.isOverloadable })
      }

      result.append(contentsOf: locals)
      space = ns.parentDeclSpace
    }

    // Handle the implicit import of the built-in module in the standard library.
    if context.isCompilingStdLib && (name == "Builtin") {
      result.types.append(context.builtin)
    }

    return result
  }

  public func lookup(qualified name: String) -> LookupResult {
    let (types, values) = localTypeAndValueDecls
    return LookupResult(
      types: types.filter({ $0.name == name }), values: values.filter({ $0.name == name }))
  }

}

extension AbstractNominalTypeDecl {

  public func lookup(qualified name: String) -> LookupResult {
    // Get the declarations directly enclosed.
    let (types, values) = localTypeAndValueDecls
    var result = LookupResult(
      types: types.filter({ $0.name == name }), values: values.filter({ $0.name == name }))

    // Merge members declared in extensions.
    for module in type.context.modules.values {
      for extDecl in module.extensions(of: self) {
        let (types, values) = extDecl.localTypeAndValueDecls
        result.types.append(contentsOf: types.filter({ $0.name == name }))
        result.values.append(contentsOf: values.filter({ $0.name == name }))
      }
    }

    // FIXME: Filter requirements without a default implementation.
    // FIXME: Merge default implementations defined in extensions.
    // Merge members inherited by conformance.
//    updateConformanceTable()
//    for conformance in conformanceTable! {
//      result.append(contentsOf: conformance.viewDecl.lookup(qualified: name))
//    }

    return result
  }

}

/// The result of a name lookup in a declaration space.
///
/// This is essentially a wrapper around two collections of type and value declarations.
public struct LookupResult {

  public init(types: [TypeDecl] = [], values: [ValueDecl] = []) {
    self.types = types
    self.values = values
  }

  /// The type declarations that were found.
  public var types: [TypeDecl]

  /// The value declarations that were found.
  public var values: [ValueDecl]

  /// Filters the lookup result with the given predicate.
  ///
  /// - Parameter isIncluded: A closure that accepts either a type or  avalue declarations and
  ///   returns whether is should be included in the returned set.
  public func filter(_ isIncluded: (TypeOrValueDecl) throws -> Bool) rethrows -> LookupResult {
    return LookupResult(
      types: try types.filter(isIncluded),
      values: try values.filter(isIncluded))
  }

  /// Appends the declarations of another lookup result.
  ///
  /// - Parameter other: Another lookup result.
  public mutating func append(contentsOf other: LookupResult) {
    types.append(contentsOf: other.types)
    values.append(contentsOf: other.values)
  }

}

extension LookupResult: Collection {

  public var startIndex: Int { 0 }

  public var endIndex: Int {
    return types.count + values.count
  }

  public func index(after position: Int) -> Int {
    return position + 1
  }

  public subscript(position: Int) -> TypeOrValueDecl {
    if position < types.count {
      return types[position]
    } else {
      return values[position - types.count]
    }
  }

}

/// An AST walker that extracts the type and value declarations in a given decl space.
fileprivate final class LocalTypeAndValueDeclFinder: NodeWalker {

  var types : [TypeDecl]  = []
  var values: [ValueDecl] = []

  override func willVisit(_ decl: Decl) -> (shouldWalk: Bool, nodeBefore: Decl) {
    if let d = decl as? TypeDecl {
      types.append(d)
    } else if let d = decl as? ValueDecl {
      values.append(d)
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
