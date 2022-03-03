import Utils

/// A pass that binds identifiers to their declaration.
public struct NameBinder: NodeWalker {

  public typealias Result = Bool

  typealias TypeSpace = TypeDecl & DeclSpace

  public enum CachedResult<T> {

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

  /// A table mapping type identifiers to their declarations.
  public var types: PropertyMap<CachedResult<TypeDecl>> = [:]

  /// The diagnostics of the binding errors.
  public var diags: [Diag] = []

  /// The modules visible from the current context.
  private let modules: [String: ModuleDecl]

  /// The module defining the standard library.
  private let stdlib: ModuleDecl?

  /// A table mapping type declarations to their member lookup tables.
  private var lookupTables: PropertyMap<[String: TypeOrValueDecl]> = [:]

  /// A table mapping type declarations to their conformance set.
  private var conformanceSets: PropertyMap<Set<ViewTypeDecl>> = [:]

  /// The generic environments of the generic declarations.
  private var environments: PropertyMap<GenericEnvironment> = [:]

  /// A set containing the extensions being currently bounded.
  ///
  /// This property is used during extension binding to avoid infinite recursion through qualified
  /// lookups into the extended type.
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
    for member in space.decls {
      if let decl = firstTypeOrValueDecl(in: member, named: name) { return decl }
    }
    return nil
  }

  private func lookup(_ name: String, qualifiedBy space: FileUnit) -> TypeOrValueDecl? {
    for member in space.decls {
      if let decl = firstTypeOrValueDecl(in: member, named: name) { return decl }
    }
    return nil
  }

  private func lookup(_ name: String, qualifiedBy space: NamespaceDecl) -> TypeOrValueDecl? {
    for member in space.decls {
      if let decl = firstTypeOrValueDecl(in: member, named: name) { return decl }
    }
    return nil
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

    // Check for members declared in conformed views.
    for view in conformanceSet(of: space) {
      // Check for a direct members.
      for member in view.directMembers {
        if let decl = check(member) { return decl }
      }

      // Check for a member declared in extensions.
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
    for member in space.decls {
      if let decl = firstTypeOrValueDecl(in: member, named: name) { return decl }
    }
    return nil
  }

  private mutating func lookup(
    _ name: String,
    qualifiedBy param: GenericParamDecl
  ) -> TypeOrValueDecl? {
    let parent = param.parentDeclSpace as! GenericTypeDecl
    let env: GenericEnvironment
    if let e = environments[parent] {
      env = e
    } else {
      env = GenericEnvironment(decl: parent, binder: &self)
      environments[parent] = env
    }

    let key = GenericEnvironment.Key([param.name])
    guard let views = env.conformanceSet(of: key) else { return nil }
    return lookup(name, in: views)
  }

  private mutating func lookup(
    _ name: String,
    in views: Set<ViewTypeDecl>
  ) -> TypeOrValueDecl? {
    for view in views {
      // Check for a direct members.
      for member in view.directMembers {
        if let decl = firstTypeOrValueDecl(in: member, named: name) { return decl }
      }

      // Check for a member declared in extensions.
      for ext in extensions(of: view) {
        for member in ext.members {
          if let decl = firstTypeOrValueDecl(in: member, named: name) { return decl }
        }
      }
    }

    // Lookup failed.
    return nil
  }

  /// Returns the first type or value declaration in `decl` with the specified name.
  private func firstTypeOrValueDecl(in decl: Decl, named name: String) -> TypeOrValueDecl? {
    switch decl {
    case let decl as TypeOrValueDecl where decl.name == name:
      return decl

    case let decl as PatternBindingDecl:
      for pattern in decl.pattern.namedPatterns where pattern.decl.name == name {
        return pattern.decl
      }

    default:
      break
    }

    return nil
  }

  /// Resolves a type identifier from the specified declaration space.
  ///
  /// - Parameters:
  ///   - ident: The type identifier to resolve
  ///   - space: The declaration space from which the unqualified lookup of the identifier's first
  ///     component should start.
  mutating func resolve(_ ident: IdentSign, unqualifiedFrom space: DeclSpace) -> TypeDecl? {
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
      assert(types[ident]?.value == nil)
      types[ident] = .failure
      diags.append(diag)
    }

    var parent: TypeDecl?
    for i in 0 ..< ident.components.count {
      let component = ident.components[i]
      let decl: TypeOrValueDecl?
      switch parent {
      case nil:
        decl = desugar(lookupResult: lookup(component.name, unqualifiedFrom: space))
      case let parent as TypeSpace:
        decl = desugar(lookupResult: lookup(component.name, qualifiedBy: parent))
      case let parent as GenericParamDecl:
        return resolve(ident, rootedAt: parent)
      default:
        fatalError("unreachable")
      }

      if let decl = decl as? TypeDecl {
        types[component] = .success(decl)
        parent = decl
      } else if let parent = parent {
        types[component] = .failure
        fail(.noType(named: component.name, in: parent, range: component.range))
        return nil
      } else {
        types[component] = .failure
        fail(.noType(named: component.name, range: component.range))
        return nil
      }
    }

    types[ident] = .success(parent!)
    return parent
  }

  /// Resolves a type identifier rooted at a generic type parameter.
  ///
  /// - Parameters:
  ///   - ident: The type identifier to resolve
  ///   - root: The declaration to which the root of the identifier refers.
  private mutating func resolve(_ ident: IdentSign, rootedAt root: GenericParamDecl) -> TypeDecl? {
    let rootParent = root.parentDeclSpace as! GenericTypeDecl
    let env: GenericEnvironment
    if let e = environments[rootParent] {
      env = e
    } else {
      env = GenericEnvironment(decl: rootParent, binder: &self)
      environments[rootParent] = env
    }

    var parent = root
    for i in 1 ..< ident.components.count {
      let component = ident.components[i]

      // Search in the context of the parent's generic environment.
      if let decl = lookup(component.name, qualifiedBy: parent) as? GenericParamDecl {
        parent = decl
        continue
      }

      // Search in the context of the root's generic environment.
      let key = GenericEnvironment.Key(ident.components[0 ..< i].map({ $0.name }))
      guard let views = env.conformanceSet(of: key) else { return nil }
      if let decl = lookup(component.name, in: views) as? GenericParamDecl {
        parent = decl
      } else {
        types[component] = .failure
        types[ident] = .failure
        diags.append(.noType(named: component.name, in: parent, range: component.range))
        return nil
      }
    }

    types[ident] = .success(parent)
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
  mutating func conformanceSet(of decl: GenericTypeDecl) -> Set<ViewTypeDecl> {
    // Check the cache.
    if let set = conformanceSets[decl] {
      return set
    }

    var set: Set<ViewTypeDecl> = []
    func insert(_ inheritance: IdentSign, unqualifiedFrom space: DeclSpace) {
      if let v = resolve(inheritance, unqualifiedFrom: space.parentDeclSpace!) as? ViewTypeDecl {
        if set.insert(v).inserted {
          set.formUnion(conformanceSet(of: v))
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
