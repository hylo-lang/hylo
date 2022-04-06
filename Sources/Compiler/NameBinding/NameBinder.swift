import Utils

/// A lookup table mapping names to their declaration.
struct NameBinder {

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

  /// The diagnostics of the binding errors.
  var diags: [Diag] = []

  /// The modules visible from the current context.
  var modules: [String: ModuleDecl]

  /// The module defining the standard library.
  ///
  /// If this property is `nil`, the name binder will not search for unqualified symbols in the
  /// standard library.
  var stdlib: ModuleDecl?

  /// A table mapping type declarations to their member lookup tables.
  private var lookupTables: [ObjectIdentifier: [String: LookupResult_]] = [:]

  /// A table mapping type declarations to their conformance set.
  private var conformanceSets: PropertyMap<Set<ViewTypeDecl>> = [:]

  /// The generic environments of the generic declarations.
  private var environments: [ObjectIdentifier: GenericEnvironment] = [:]

  /// A table mapping type names to their declarations.
  private var types: PropertyMap<CachedResult<TypeDecl>> = [:]

  /// A set containing the extensions being currently bounded.
  ///
  /// This property is used during extension binding to avoid infinite recursion through qualified
  /// lookups into the extended type.
  private var extensionsUnderBinding: Set<ObjectIdentifier> = []

  init(modules: [String: ModuleDecl], stdlib: ModuleDecl?) {
    self.modules = modules
    self.stdlib = stdlib
  }

  mutating func lookup(
    _ name: String,
    unqualifiedFrom space: DeclSpace,
    initialMatches: LookupResult_ = LookupResult_()
  ) -> LookupResult_ {
    // Check if the name defines a built-in alias.
    switch name {
    case "Any": return LookupResult_(type: AliasTypeDecl.any)
    case "Nothing": return LookupResult_(type: AliasTypeDecl.nothing)
    default:
      break
    }

    var space = space
    var matches = initialMatches

    while true {
      // Search for the name in the current space.
      for decl in lookup(name, qualifiedBy: space) {
        matches.insert(decl)
      }

      // We're done if we found a non-overloadable symbol.
      guard matches.allOverloadable else { return matches }

      // Move to the parent.
      if let parent = space.parentDeclSpace {
        space = parent
      } else if let stdlib = stdlib, space !== stdlib {
        space = stdlib
      } else {
        break
      }
    }

    // Search for the name in the set of visible modules.
    if let module = modules[name] {
      matches.insert(module)
    }

    return matches
  }

  mutating func lookup(_ name: String, qualifiedBy space: DeclSpace) -> LookupResult_ {
    switch space {
    case let space as ModuleDecl:
      return lookup(name, qualifiedByIterable: space)
    case let space as FileUnit:
      return lookup(name, qualifiedByIterable: space)
    case let space as NamespaceDecl:
      return lookup(name, qualifiedByIterable: space)
    case let space as BraceStmt:
      return lookup(name, qualifiedByIterable: space)
    case let space as GenericTypeDecl:
      return lookup(name, qualifiedBy: space)
    case let space as ExtensionDecl:
      return lookup(name, qualifiedBy: space)
    case let space as BaseFunDecl:
      return lookup(name, qualifiedBy: space)
    default:
      fatalError("unexpected declaration space '\(type(of: space))'")
    }
  }

  mutating func lookup<S>(_ name: String, qualifiedByIterable space: S) -> LookupResult_
  where S: IterableDeclSpace
  {
    if let table = lookupTables[ObjectIdentifier(space)] {
      return table[name, default: LookupResult_()]
    }

    var table: [String: LookupResult_] = [:]
    for member in space.decls {
      NameBinder.register(member, into: &table)
    }

    lookupTables[ObjectIdentifier(space)] = table
    return table[name, default: LookupResult_()]
  }

  mutating func lookup(_ name: String, qualifiedBy space: GenericTypeDecl) -> LookupResult_ {
    if let t = lookupTables[ObjectIdentifier(space)] {
      // We already initialized a lookup table. If it contains an entry for `name`, we're done.
      if let r = t[name] { return r }
    } else {
      // Initialize the lookup table with members introduced by the declaration.
      var table: [String: LookupResult_] = [:]
      defer { lookupTables[ObjectIdentifier(space)] = table }

      // Register direct members.
      for member in space.directMembers {
        NameBinder.register(member, into: &table)
      }

      // Register type parameters introduced by the declaration.
      if let clause = space.genericClause {
        for param in clause.params {
          NameBinder.register(param, into: &table)
        }
      }

      switch space {
      case let d as AliasTypeDecl where d.aliasedSign is NameSign:
        // No more members to check if the declaration denotes a type synonm.
        return table[name, default: LookupResult_()]

      case let d as ViewTypeDecl:
        // In views, `Self` is a type parameter.
        table["Self"] = LookupResult_(type: d.selfTypeDecl)

      case let d as ProductTypeDecl:
        // In nominal product types, `Self` is an alias for the declaration.
        table["Self"] = LookupResult_(type: d)

      default:
        break
      }
    }

    // Complete the table with with extensions and conformances. This must be done lazily in case
    // we re-enter `lookup(_:qualifiedBy:)` to resolve the extended name of an extension.
    for ext in extensions(of: space) {
      modify(value: &lookupTables[ObjectIdentifier(space)]!, with: { table in
        for member in ext.directMembers {
          NameBinder.register(member, into: &table)
        }
      })
    }

    // Register members declared in conformed views.
    for view in conformanceSet(of: space) {
      // Register direct members.
      modify(value: &lookupTables[ObjectIdentifier(space)]!, with: { table in
        for member in view.directMembers {
          NameBinder.register(member, into: &table)
        }
      })

      // Register members declared in extensions.
      for ext in extensions(of: view) {
        modify(value: &lookupTables[ObjectIdentifier(space)]!, with: { table in
          for member in ext.directMembers {
            NameBinder.register(member, into: &table)
          }
        })
      }
    }

    // Nominal product types get a default constructor if none has been defined.
    if let space = space as? NominalTypeDecl {
      modify(value: &lookupTables[ObjectIdentifier(space)]!, with: { table in
        NameBinder.register(space.synthesizeDefaultCtor(), into: &table)
      })
    }

    return lookupTables[ObjectIdentifier(space)]![name, default: LookupResult_()]
  }

  mutating func lookup(_ name: String, qualifiedBy space: ExtensionDecl) -> LookupResult_ {
    if let d = resolve(
      space.extendedName, unqualifiedFrom: space.parentDeclSpace!) as? GenericTypeDecl
    {
      return lookup(name, qualifiedBy: d)
    } else {
      return LookupResult_()
    }
  }

  mutating func lookup(_ name: String, qualifiedBy space: BaseFunDecl) -> LookupResult_ {
    // Check the cache.
    if let t = lookupTables[ObjectIdentifier(space)] {
      return t[name, default: LookupResult_()]
    }
    var table: [String: LookupResult_] = [:]
    defer { lookupTables[ObjectIdentifier(space)] = table }

    // Register function function parameters.
    for param in space.params {
      NameBinder.register(param, into: &table)
    }

    // Register `self` in member functions.
    if let decl = space.selfDecl {
      NameBinder.register(decl, into: &table)
    }

    // Register type parameters introduced by the declaration.
    if let clause = space.genericClause {
      for param in clause.params {
        NameBinder.register(param, into: &table)
      }
    }

    // Register captures explicitly declared in the capture-list.
    for capture in space.explicitCaptures {
      NameBinder.register(capture, into: &table)
    }

    return table[name, default: LookupResult_()]
  }

  mutating func lookup(_ name: String, qualifiedBy param: GenericParamDecl) -> LookupResult_ {
    let parent = param.parentDeclSpace as! BaseGenericDecl
    let env = environment(of: parent)

    let key = GenericEnvironment.Key([param.ident])
    guard let views = env.conformanceSet(of: key) else { return LookupResult_() }
    return lookup(name, in: views)
  }

  mutating func lookup(_ name: String, in views: Set<ViewTypeDecl>) -> LookupResult_ {
    var result = LookupResult_()

    for view in views {
      // Check for a direct members.
      for member in view.directMembers {
        mergeTypeOrValueDecl(of: member, named: name, into: &result)
        guard result.allOverloadable else { return result }
      }

      // Check for a member declared in extensions.
      for ext in extensions(of: view) {
        for member in ext.directMembers {
          mergeTypeOrValueDecl(of: member, named: name, into: &result)
          guard result.allOverloadable else { return result }
        }
      }
    }

    return result
  }

  /// Merges the type and value declarations in `decl` with the specified name into `result`.
  private func mergeTypeOrValueDecl(
    of decl: Decl,
    named name: String,
    into result: inout LookupResult_
  ) {
    switch decl {
    case let decl as TypeOrValueDecl where decl.ident == name:
      result.insert(decl)

    case let decl as PatternBindingDecl:
      for pattern in decl.pattern.namedPatterns where pattern.decl.ident == name {
        result.insert(pattern.decl)
      }

    default:
      break
    }
  }

  /// Resolves a type name from the specified declaration space.
  ///
  /// - Parameters:
  ///   - name: The type name to resolve
  ///   - space: The declaration space from which the unqualified lookup of the name's first
  ///     component should start.
  mutating func resolve(_ name: NameSign, unqualifiedFrom space: DeclSpace) -> TypeDecl? {
    if let cache = types[name] {
      return cache.value
    }

    func desugar(lookupResult decl: TypeOrValueDecl?) -> TypeOrValueDecl? {
      if let a = decl as? AliasTypeDecl,
         let i = a.aliasedSign as? NameSign
      {
        // If the component is a synonym, we should resolve it.
        return resolve(i, unqualifiedFrom: a)
      } else {
        return decl
      }
    }

    func fail(_ diag: Diag) {
      assert(types[name]?.value == nil)
      types[name] = .failure
      diags.append(diag)
    }

    var parent: TypeDecl?
    for i in 0 ..< name.components.count {
      let component = name.components[i]
      let decl: TypeOrValueDecl?
      switch parent {
      case let parent as GenericParamDecl:
        return resolve(name, rootedAt: parent)
      case let parent as DeclSpace:
        decl = desugar(lookupResult: lookup(component.ident, qualifiedBy: parent).type)
      default:
        decl = desugar(lookupResult: lookup(component.ident, unqualifiedFrom: space).type)
      }

      if let decl = decl as? TypeDecl {
        types[component] = .success(decl)
        parent = decl
      } else if let parent = parent {
        types[component] = .failure
        fail(.noType(named: component.ident, in: parent, range: component.range))
        return nil
      } else {
        types[component] = .failure
        fail(.noType(named: component.ident, range: component.range))
        return nil
      }
    }

    types[name] = .success(parent!)
    return parent
  }

  /// Resolves a type name rooted at a generic type parameter.
  ///
  /// - Parameters:
  ///   - name: The type name to resolve
  ///   - root: The declaration to which the root of the identifier refers.
  private mutating func resolve(_ name: NameSign, rootedAt root: GenericParamDecl) -> TypeDecl? {
    let rootParent = root.parentDeclSpace as! BaseGenericDecl & Node
    let env = environment(of: rootParent)

    var parent = root
    for i in 1 ..< name.components.count {
      let component = name.components[i]

      // Search in the context of the parent's generic environment.
      if let decl = lookup(component.ident, qualifiedBy: parent).type as? GenericParamDecl {
        parent = decl
        continue
      }

      // Search in the context of the root's generic environment.
      let key = GenericEnvironment.Key(name.components[0 ..< i].map({ $0.ident }))
      guard let views = env.conformanceSet(of: key) else {
        types[component] = .failure
        types[name] = .failure
        diags.append(.noType(named: component.ident, in: parent, range: component.range))
        return nil
      }

      if let decl = lookup(component.ident, in: views).type as? GenericParamDecl {
        parent = decl
      } else {
        types[component] = .failure
        types[name] = .failure
        diags.append(.noType(named: component.ident, in: parent, range: component.range))
        return nil
      }
    }

    types[name] = .success(parent)
    return parent
  }

  /// Returns the extensions of `decl`.
  mutating func extensions(of decl: GenericTypeDecl) -> [ExtensionDecl] {
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
        let d = resolve(ext.extendedName, unqualifiedFrom: root)
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
    func insert(_ inheritance: NameSign, unqualifiedFrom space: DeclSpace) {
      if let v = resolve(inheritance, unqualifiedFrom: space.parentDeclSpace!) as? ViewTypeDecl {
        if set.insert(v).inserted {
          set.formUnion(conformanceSet(of: v))
        }
      }
    }

    // Populate the set with direct conformance declarations.
    for case let i as NameSign in decl.inheritances {
      insert(i, unqualifiedFrom: decl)
    }

    // Populate the set with conformance declared in extensions.
    for ext in extensions(of: decl) {
      for case let i as NameSign in ext.inheritances {
        insert(i, unqualifiedFrom: ext)
      }
    }

    conformanceSets[decl] = set
    return set
  }

  /// Returns the generic environment of `decl`.
  mutating func environment(of decl: BaseGenericDecl) -> GenericEnvironment {
    if let e = environments[ObjectIdentifier(decl)] {
      return e
    } else {
      let e = GenericEnvironment(decl: decl, binder: &self)
      environments[ObjectIdentifier(decl)] = e
      return e
    }
  }

  /// Returns the generic environment introducing `decl`.
  mutating func environment(defining decl: GenericParamDecl) -> GenericEnvironment? {
    var space = decl.parentDeclSpace?.innermostGenericSpace
    while let s = space {
      let e = environment(of: s)
      if e.params.contains(where: { $0.id == decl.id }) {
        return e
      } else {
        space = s.parentDeclSpace?.innermostGenericSpace
      }
    }
    return nil
  }

  /// Registers the declarations introduced by `member` into `table`.
  private static func register(_ member: Decl, into table: inout [String: LookupResult_]) {
    switch member {
    case let decl as TypeOrValueDecl:
      _ = modify(
        value: &table[decl.ident, default: LookupResult_()],
        with: { $0.insert(decl) })

    case let decl as PatternBindingDecl:
      for pat in decl.pattern.namedPatterns {
        _ = modify(
          value: &table[pat.decl.ident, default: LookupResult_()],
          with: { $0.insert(pat.decl) })
      }

    default:
      break
    }
  }

}

extension NominalTypeDecl {

  /// Synthetizes a default constructor declaration.
  fileprivate func synthesizeDefaultCtor() -> CtorDecl {
    let ctor = CtorDecl(type: .unresolved)
    ctor.range = introRange
    ctor.parentDeclSpace = self
    ctor.props.insert(.isSynthesized)

    // Create a parameter for each stored property.
    ctor.params = storedVars.map({ (varDecl: VarDecl) -> FunParamDecl in
      let param = FunParamDecl(
        ident: varDecl.ident,
        label: varDecl.ident,
        policy: .consuming,
        type: .unresolved)
      param.parentDeclSpace = ctor
      return param
    })

    return ctor
  }

}
