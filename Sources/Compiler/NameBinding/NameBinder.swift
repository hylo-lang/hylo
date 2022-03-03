import Utils

/// A pass that binds identifiers to their declaration.
public struct NameBinder: NodeWalker {

  public typealias Result = Bool

  typealias TypeSpace = TypeDecl & DeclSpace

  private enum CachedResult<T> {

    case success(T)

    case failure

    var value: T? {
      if case .success(let value) = self {
        return value
      } else {
        return nil
      }
    }

  }

  public var parent: Node?

  public var innermostSpace: DeclSpace?

  /// The diagnostics of the binding errors.
  public var diags: [Diag] = []

  /// The modules visible from the current context.
  let modules: [String: ModuleDecl]

  /// The module defining the standard library.
  let stdlib: ModuleDecl?

  /// A table mapping type declarations to their member lookup tables.
  private var lookupTables: PropertyMap<[String: TypeOrValueDecl]> = [:]

  /// A table mapping type declarations to their conformance set.
  private var conformanceSets: PropertyMap<Set<ViewTypeDecl>> = [:]

  /// A table mapping type identifiers to their declarations.
  private var types: PropertyMap<CachedResult<TypeDecl>> = [:]

  /// A set containing the extensions being currently bounded.
  private var extensionsUnderBinding: Set<ObjectIdentifier> = []

  public init(modules: [String: ModuleDecl], stdlib: ModuleDecl?) {
    self.modules = modules
    self.stdlib = stdlib
  }

  public mutating func willVisit(_ sign: Sign) -> (shouldWalk: Bool, nodeBefore: Sign) {
    // FIXME we should resolve signatures from the nodes containing them, because the declaration
    // space from which they are resolved might depend on their position in the node.

    if let ident = sign as? IdentSign {
      let a = resolve(ident, unqualifiedFrom: innermostSpace!)
      print(a?.name as Any)
      return (shouldWalk: false, nodeBefore: ident)
    }

    return (shouldWalk: true, nodeBefore: sign)
  }

  private mutating func lookup(
    _ name: String,
    unqualifiedFrom space: DeclSpace
  ) -> TypeOrValueDecl? {
    var space = space
    while true {
      // Search for the name in the current space.
      if let local = lookup(name, qualifiedBy: space) {
        return local
      }

      // Move to the parent.
      if let parent = space.parentDeclSpace {
        space = parent
      } else if let stdlib = stdlib, space !== stdlib {
        space = stdlib
      } else {
        return nil
      }
    }
  }

  private mutating func lookup(_ name: String, qualifiedBy space: DeclSpace) -> TypeOrValueDecl? {
    switch space {
    case let space as ModuleDecl:
      return lookup(name, qualifiedBy: space)
    case let space as FileUnit:
      return lookup(name, qualifiedBy: space)
    case let space as NamespaceDecl:
      return lookup(name, qualifiedBy: space)
    case let space as GenericTypeDecl:
      return lookup(name, qualifiedBy: space)
    case let space as ExtensionDecl:
      return lookup(name, qualifiedBy: space)
    case let space as BraceStmt:
      return lookup(name, qualifiedBy: space)
    default:
      fatalError("unexpected declaration space '\(type(of: space))'")
    }
  }

  private func lookup(_ name: String, qualifiedBy space: ModuleDecl) -> TypeOrValueDecl? {
    return space.decls.first(transformedBy: { some($0, whereNameIs: name) })
  }

  private func lookup(_ name: String, qualifiedBy space: FileUnit) -> TypeOrValueDecl? {
    return space.decls.first(transformedBy: { some($0, whereNameIs: name) })
  }

  private func lookup(_ name: String, qualifiedBy space: NamespaceDecl) -> TypeOrValueDecl? {
    return space.decls.first(transformedBy: { some($0, whereNameIs: name) })
  }

  private mutating func lookup(
    _ name: String,
    qualifiedBy space: GenericTypeDecl
  ) -> TypeOrValueDecl? {
    // Check the cache.
    if let result = lookupTables[space, default: [:]][name] {
      return result
    }

    /// Checks if `member` defines a value or type named `name`.
    func check(_ member: Decl) -> TypeOrValueDecl? {
      switch member {
      case let decl as TypeOrValueDecl:
        lookupTables[space, default: [:]][decl.name] = decl
        if decl.name == name { return decl }
      case let decl as PatternBindingDecl:
        for pattern in decl.pattern.namedPatterns {
          lookupTables[space, default: [:]][pattern.decl.name] = pattern.decl
          if pattern.decl.name == name { return pattern.decl }
        }
      default:
        break
      }
      return nil
    }

    // Check for a direct members.
    for member in space.directMembers {
      if let decl = check(member), decl.name == name { return decl }
    }

    // Check for a type parameters introduced by the declaration.
    if let clause = space.genericClause {
      for param in clause.params {
        lookupTables[space, default: [:]][param.name] = param
        if param.name == name { return param }
      }
    }

    // Lookup failed if the declaration denotes a type synonym.
    guard !((space as? AliasTypeDecl)?.aliasedSign is IdentSign) else { return nil }

    // Check for a member declared in extensions.
    for ext in extensions(of: space) {
      for member in ext.members {
        if let decl = check(member) { return decl }
      }
    }

    // Populate the table with members declared in conformed views.
    for view in conformanceSet(of: space) {
      // Insert direct members.
      for member in view.directMembers {
        if let decl = check(member) { return decl }
      }

      // Insert members declared in extensions.
      for ext in extensions(of: view) {
        for member in ext.members {
          if let decl = check(member) { return decl }
        }
      }
    }

    // Lookup failed.
    return nil
  }

  private mutating func lookup(
    _ name: String,
    qualifiedBy space: ExtensionDecl
  ) -> TypeOrValueDecl? {
    if let d = resolve(
      space.extendedIdent, unqualifiedFrom: space.parentDeclSpace!) as? GenericTypeDecl
    {
      return lookup(name, qualifiedBy: d)
    } else {
      return nil
    }
  }

  private func lookup(_ name: String, qualifiedBy space: BraceStmt) -> TypeOrValueDecl? {
    return space.decls.first(transformedBy: { some($0, whereNameIs: name) })
  }

  private func some(_ decl: Decl, whereNameIs name: String) -> TypeOrValueDecl? {
    switch decl {
    case let decl as TypeDecl where decl.name == name:
      return decl
    case let decl as ValueDecl where decl.name == name:
      return decl
    case let decl as PatternBindingDecl:
      return decl.pattern.namedPatterns.first(where: { $0.decl.name == name })?.decl
    default:
      return nil
    }
  }

  /// Resolves a type identifier from the specified declaration space.
  ///
  /// - Parameters:
  ///   - ident: The type identifier to resolve
  ///   - space: The declaration space from which the unqualified lookup of the identifier's first
  ///     component should start.
  private mutating func resolve(
    _ ident: IdentSign,
    unqualifiedFrom space: DeclSpace
  ) -> TypeDecl? {
    if let cache = types[ident] {
      return cache.value
    }

    func desugar(lookupResult decl: TypeOrValueDecl?) -> TypeOrValueDecl? {
      if let a = decl as? AliasTypeDecl,
         let i = a.aliasedSign as? IdentSign
      {
        // If the component is a synonym, we should resolve it.
        return resolve(i, unqualifiedFrom: a)
      } else {
        return decl
      }
    }

    func fail(_ diag: Diag) {
      assert(types[ident] == nil)
      diags.append(diag)
      types[ident] = .failure
    }

    var parent: TypeSpace?
    for i in 0 ..< ident.components.count {
      let component = ident.components[i]
      let decl: TypeOrValueDecl?
      if let parent = parent {
        decl = desugar(lookupResult: lookup(component.name, qualifiedBy: parent))
      } else {
        decl = desugar(lookupResult: lookup(component.name, unqualifiedFrom: space))
      }

      switch decl {
      case let decl as TypeSpace:
        parent = decl

      case let decl as TypeDecl:
        guard i == ident.components.count - 1 else {
          let next = ident.components[i + 1]
          fail(.noType(named: next.name, in: decl, range: next.range))
          return nil
        }

        types[ident] = .success(decl)
        return decl

      default:
        if let parent = parent {
          fail(.noType(named: component.name, in: parent, range: component.range))
        } else {
          fail(.noType(named: component.name, range: component.range))
        }
        return nil
      }
    }

    types[ident] = .success(parent!)
    return parent
  }

  /// Returns the extensions of `decl`.
  private mutating func extensions(of decl: GenericTypeDecl) -> [ExtensionDecl] {
    // Loop through each module.
    let root = decl.rootDeclSpace
    var matches: [ExtensionDecl] = []
    for module in modules.values {
      for case let ext as ExtensionDecl in module.decls {
        // Skip extensions that are being bound.
        guard extensionsUnderBinding.insert(ObjectIdentifier(ext)).inserted else { continue }
        defer { extensionsUnderBinding.remove(ObjectIdentifier(ext)) }

        // Bind the extension's identifier. Since extensions are always declared at the top-level,
        // we can start unqualified lookups from the module.
        let d = resolve(ext.extendedIdent, unqualifiedFrom: root)
        if d === decl {
          matches.append(ext)
        }
      }
    }

    return matches
  }

  /// Returns the set of views to which `decl` conforms.
  private mutating func conformanceSet(of decl: GenericTypeDecl) -> Set<ViewTypeDecl> {
    // Check the cache.
    if let set = conformanceSets[decl] {
      return set
    }

    var set: Set<ViewTypeDecl> = []
    func insert(_ inheritance: IdentSign, unqualifiedFrom space: DeclSpace) {
      if let v = resolve(inheritance, unqualifiedFrom: space.parentDeclSpace!) as? ViewTypeDecl {
        if set.insert(v).inserted {
          for case let i as IdentSign in v.inheritances {
            insert(i, unqualifiedFrom: v)
          }
        }
      }
    }

    // Populate the set with direct conformance declarations.
    for case let i as IdentSign in decl.inheritances {
      insert(i, unqualifiedFrom: decl)
    }

    // Populate the set with conformance declared in extensions.
    for ext in extensions(of: decl) {
      for case let i as IdentSign in ext.inheritances {
        insert(i, unqualifiedFrom: ext)
      }
    }

    conformanceSets[decl] = set
    return set
  }

}
