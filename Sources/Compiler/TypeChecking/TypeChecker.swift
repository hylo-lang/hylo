import Utils

/// Val's type checker.
public struct TypeChecker {

  /// The AST containing the modules being type checked.
  private var ast: AST

  /// The scope hierarchy of the AST.
  private let scopeHierarchy: ScopeHierarchy

  /// The diagnostics of the type errors.
  public private(set) var diags: [Diag] = []

  /// Creates a new type checker for the specified AST.
  ///
  /// - Note: `ast` is stored in the type checker and mutated throughout type checking (e.g., to
  ///   insert synthesized declarations).
  public init(ast: AST) {
    self.ast = ast
    self.scopeHierarchy = ast.scopeHierarchy()
  }

  // MARK: Type canonicalization

  /// Returns the canonical form of `type`.
  public func canonicalize(type: Type) -> Type {
    if type[.isCanonical] { return type }

    switch type {
    case .boundGeneric(let t):
      let base = canonicalize(type: t.base)
      let arguments = t.arguments.map({ (a) -> BoundGenericType.Argument in
        switch a {
        case .size:
          fatalError("not implemented")
        case .type(let a):
          return .type(canonicalize(type: a))
        }
      })
      return .boundGeneric(BoundGenericType(base, arguments: arguments))

    case .existential(let t):
      return .existential(ExistentialType(
        traits: t.traits,
        constraints: Set(t.constraints.map(canonicalize(constraint:)))))

    case .tuple(let t):
      return .tuple(TupleType(
        t.elements.map({ element in
          TupleType.Element(label: element.label, type: canonicalize(type: element.type))
        })))

    case .union(let t):
      return .union(UnionType(elements: Set(t.elements.map(canonicalize(type:)))))

    default:
      unreachable()
    }
  }

  /// Returns the canonical form of `constraint`.
  public func canonicalize(constraint: TypeConstraint) -> TypeConstraint {
    var canonical = constraint
    canonical.visitTypes({ $0 = canonicalize(type: $0) })
    return canonical
  }

  // MARK: Type checking

  /// The status of a type checking request on a declaration.
  private enum RequestStatus {

    /// Type realization has started.
    ///
    /// The type checker is realizing the overarching type of the declaration. Initiating a new
    /// type realization or type checking request on the same declaration will cause a circular
    /// dependency error.
    case typeRealizationStarted

    /// Type realization was completed.
    ///
    /// The checker realized the overarching type of the declaration, which is now available in
    /// `declTypes`.
    case typeRealizationCompleted

    /// Type checking has started.
    ///
    /// The checker is verifying whether the declaration is well-formed; its overarching type is
    /// available in `declTypes`. Initiating a new type checking request will cause a circular
    /// dependency error.
    case typeCheckingStarted

    /// Type checking succeeded.
    ///
    /// The declaration is well-formed; its overarching type is availabe in `declTypes`.
    case success

    /// Type realzation or type checking failed.
    ///
    /// If type realization succeeded, the overarching type of the declaration is available in
    /// `declTypes`. Otherwise, it is assigned to `nil`.
    case failure

  }

  /// A cache for type checking requests on declarations.
  private var declRequests = DeclMap<RequestStatus>()

  /// A cache mapping declarations to the constraints extracted from their generic clauses.
  private var clauseConstraints = DeclMap<[TypeConstraint]?>()

  /// Type checks the specified module and returns whether that succeeded.
  ///
  /// - Requires: `i` is a valid ID in the type checker's AST.
  public mutating func check(module i: NodeID<ModuleDecl>) -> Bool {
    // Build the type of the module.
    declTypes[i] = .module(ModuleType(decl: i, ast: ast))

    // Type check the declarations in the module.
    return ast[i].members.reduce(true, { (success, j) in
      check(decl: j) && success
    })
  }

  /// Type checks the specified declaration and returns whether that succeeded.
  private mutating func check<T: DeclID>(decl i: T) -> Bool {
    switch i.kind {
    case .associatedSizeDecl:
      return check(associatedSize: NodeID(converting: i)!)
    case .associatedTypeDecl:
      return check(associatedType: NodeID(converting: i)!)
    case .traitDecl:
      return check(trait: NodeID(converting: i)!)
    case .typeAliasDecl:
      return check(typeAlias: NodeID(converting: i)!)
    default:
      unreachable("unexpected declaration")
    }
  }

  private mutating func check(associatedSize: NodeID<AssociatedSizeDecl>) -> Bool {
    return true
  }

  private mutating func check(associatedType: NodeID<AssociatedTypeDecl>) -> Bool {
    return true
  }

  private mutating func check(binding: BindingDecl) -> Bool {
    fatalError("not implemented")
  }

  private mutating func check(conformance: ConformanceDecl) -> Bool {
    fatalError("not implemented")
  }

  private mutating func check(extension: ExtensionDecl) -> Bool {
    fatalError("not implemented")
  }

  private mutating func check(fun: FunDecl) -> Bool {
    fatalError("not implemented")
  }

  private mutating func check(genericSizeParam: GenericSizeParamDecl) -> Bool {
    fatalError("not implemented")
  }

  private mutating func check(genericTypeParam: GenericTypeParamDecl) -> Bool {
    fatalError("not implemented")
  }

  private mutating func check(methodImpl: MethodImplDecl) -> Bool {
    fatalError("not implemented")
  }

  private mutating func check(namespace: NamespaceDecl) -> Bool {
    fatalError("not implemented")
  }

  private mutating func check(param: ParamDecl) -> Bool {
    fatalError("not implemented")
  }

  private mutating func check(productType: ProductTypeDecl) -> Bool {
    fatalError("not implemented")
  }

  private mutating func check(subscript: SubscriptDecl) -> Bool {
    fatalError("not implemented")
  }

  private mutating func check(subscriptImpl: SubscriptImplDecl) -> Bool {
    fatalError("not implemented")
  }

  private mutating func check(trait i: NodeID<TraitDecl>) -> Bool {
    _check(decl: i, { (this, i) in
      // Type check the generic constraints of the declaration.
      var success = this.constraints(ofTrait: i) != nil

      // Type check the type's direct members.
      for j in this.ast[i].members {
        success = this.check(decl: j) && success
      }

      // Type check extensions.
      let type = this.declTypes[i]!!
      for j in this.extensions(of: type, exposedTo: this.scopeHierarchy.container[i]!) {
        success = this.check(decl: j) && success
      }

      // TODO: Check the conformances

      return success
    })
  }

  private mutating func check(typeAlias i: NodeID<TypeAliasDecl>) -> Bool {
    _check(decl: i, { (this, i) in
      // Realize the subject of the declaration.
      let subject: Type
      switch this.ast[i].body.value {
      case .typeExpr(let j):
        if let s = this.realize(j, inScope: AnyNodeID(i)) {
          subject = s
        } else {
          return false
        }

      case.union:
        fatalError("not implemented")
      }

      // Type-check the generic clause of the declaration.
      var success = this.constraints(of: i) != nil

      // Type check extensions.
      for j in this.extensions(of: subject, exposedTo: this.scopeHierarchy.container[i]!) {
        success = this.check(decl: j) && success
      }

      // TODO: Check the conformances

      return success
    })
  }

  private mutating func check(`var`: VarDecl) -> Bool {
    fatalError("not implemented")
  }

  /// Returns whether `decl` is well-typed from the cache, or calls `action` to type check it
  /// and caches the result before returning it.
  private mutating func _check<T: DeclID>(
    decl i: T,
    _ action: (inout Self, T) -> Bool
  ) -> Bool {
    // Check if a type checking request has already been received.
    while true {
      switch declRequests[i] {
      case nil:
        // Realize the type of the declaration before starting type checking.
        _  = realize(decl: i)
        assert(declRequests[i] != nil)
        continue

      case .typeRealizationCompleted:
        declRequests[i] = .typeCheckingStarted

      case .typeRealizationStarted, .typeCheckingStarted:
        diags.append(.circularDependency(range: ast.ranges[i]))
        return false

      case .success:
        return true

      case .failure:
        return false
      }

      break
    }

    // Process the request.
    let success = action(&self, i)

    // Update the request status.
    declRequests[i] = success ? .success : .failure
    return success
  }

  /// Returns the constraints on the generic parameters of defined by `decl`'s generic clause, or
  /// `nil` if those constraints are ill-formed.
  ///
  /// - Note: Call `constraints(ofTrait)` instead of this method to collect the constraints of a
  ///   trait declaration.
  private mutating func constraints<T: GenericDecl>(of i: NodeID<T>) -> [TypeConstraint]? {
    assert(T.self != TraitDecl.self)
    if let constraints = clauseConstraints[i] {
      return constraints
    }

    // Nothing to do if the declaration has no generic clause.
    guard let clause = ast[i].genericClause?.value else {
      clauseConstraints[i] = []
      return []
    }

    let scope = AnyNodeID(i)
    var success = true
    var constraints: [TypeConstraint] = []

    // Realize the traits in the conformance lists of each generic parameter.
    for case .type(let j) in clause.params {
      // Realize the generic type parameter.
      guard let lhs = realize(decl: j) else {
        success = false
        continue
      }
      assert(lhs.base is GenericTypeParamType)

      // Synthesize the sugared conformance constraint, if any.
      let list = ast[j].conformances
      if let c = synthesizeConformanceConstraint(expressedBy: list, on: lhs, inScope: scope) {
        constraints.append(c)
      }
    }

    // Evaluate the constraint expressions of the associated type's where clause.
    if let whereClause = clause.whereClause?.value {
      for expr in whereClause.constraints {
        if let constraint = eval(constraintExpr: expr, inScope: scope) {
          constraints.append(constraint)
        } else {
          success = false
        }
      }
    }

    if success {
      clauseConstraints[i] = constraints
      return constraints
    } else {
      clauseConstraints[i] = nil
      return nil
    }
  }

  /// Returns the constraints on the types conforming to `decl` and its associated types, or `nil`
  /// if those constraints are ill-formed.
  private mutating func constraints(ofTrait i: NodeID<TraitDecl>) -> [TypeConstraint]? {
    if let constraints = clauseConstraints[i] {
      return constraints
    }

    var success = true
    var constraints: [TypeConstraint] = []

    // Collect and type check the constraints defined on associated types and sizes.
    for member in ast[i].members {
      switch member.kind {
      case .associatedSizeDecl:
        success = associatedConstraints(
          ofSize: NodeID(converting: member)!,
          ofTrait: i,
          into: &constraints) && success

      case .associatedTypeDecl:
        success = associatedConstraints(
          ofType: NodeID(converting: member)!,
          ofTrait: i,
          into: &constraints) && success

      default:
        continue
      }
    }

    // Bail out if we found ill-form constraints.
    if !success {
      clauseConstraints[i] = nil
      return nil
    }

    // Synthesize `Self: T`.
    let trait = declTypes[i]!!.base as! TraitType
    let selfType = GenericTypeParamType(decl: i, ast: ast)
    constraints.append(.conformance(l: .genericTypeParam(selfType), traits: [.trait(trait)]))

    clauseConstraints[i] = constraints
    return constraints
  }

  // Evaluates the constraints declared in `associatedSize`, stores them in `constraints` and
  // returns whether they are all well-formed.
  private mutating func associatedConstraints(
    ofSize associatedSize: NodeID<AssociatedSizeDecl>,
    ofTrait trait: NodeID<TraitDecl>,
    into constraints: inout [TypeConstraint]
  ) -> Bool {
    // Realize the generic type parameter corresponding to the associated size.
    guard let lhs = realize(decl: associatedSize) else { return false }
    assert(lhs.base is GenericSizeParamType)

    var success = true

    // Evaluate the constraint expressions of the associated size's where clause.
    if let whereClause = ast[associatedSize].whereClause?.value {
      for expr in whereClause.constraints {
        if let constraint = eval(constraintExpr: expr, inScope: AnyNodeID(trait)) {
          constraints.append(constraint)
        } else {
          success = false
        }
      }
    }

    return success
  }

  // Evaluates the constraints declared in `associatedType`, stores them in `constraints` and
  // returns whether they are all well-formed.
  private mutating func associatedConstraints(
    ofType associatedType: NodeID<AssociatedTypeDecl>,
    ofTrait trait: NodeID<TraitDecl>,
    into constraints: inout [TypeConstraint]
  ) -> Bool {
    // Realize the generic type parameter corresponding to the associated type.
    guard let lhs = realize(decl: associatedType) else { return false }
    assert(lhs.base is GenericTypeParamType)

    // Synthesize the sugared conformance constraint, if any.
    let list = ast[associatedType].conformances
    if let c = synthesizeConformanceConstraint(expressedBy: list, on: lhs, inScope: AnyNodeID(trait)) {
      constraints.append(c)
    }

    // Evaluate the constraint expressions of the associated type's where clause.
    var success = true
    if let whereClause = ast[associatedType].whereClause?.value {
      for expr in whereClause.constraints {
        if let constraint = eval(constraintExpr: expr, inScope: AnyNodeID(trait)) {
          constraints.append(constraint)
        } else {
          success = false
        }
      }
    }

    return success
  }

  /// Synthesize the sugared conformance constraint expressed by the type expressions in
  /// `conformances` on `lhs`.
  private mutating func synthesizeConformanceConstraint(
    expressedBy conformances: [NodeID<NameTypeExpr>],
    on lhs: Type,
    inScope scope: AnyNodeID
  ) -> TypeConstraint? {
    // Realize the traits in the conformance list.
    var traits: Set<Type> = []
    for expr in conformances {
      guard let rhs = realize(name: expr, inScope: scope) else { return nil }

      /// RHS must be a trait.
      if rhs.base is TraitType {
        traits.insert(rhs)
      } else {
        diags.append(.conformanceToNonTraitType(rhs, range: ast.ranges[expr]))
        return nil
      }
    }

    // Create a constraint for the traits in the conformance list.
    if traits.isEmpty {
      return nil
    } else {
      return .conformance(l: lhs, traits: traits)
    }
  }

  /// Evaluates `expr` in `scope` and returns a type constraint, or `nil` if evaluation failed.
  ///
  /// - Note: Calling this method multiple times with the same arguments may duplicate diagnostics.
  private mutating func eval(
    constraintExpr expr: SourceRepresentable<WhereClause.ConstraintExpr>,
    inScope scope: AnyNodeID
  ) -> TypeConstraint? {
    assert(ast[scope] is LexicalScope)

    switch expr.value {
    case .equality(let l, let r):
      guard let a = realize(l, inScope: scope) else { return nil }
      guard let b = realize(r, inScope: scope) else { return nil }

      if !a.isTypeParam && !b.isTypeParam {
        diags.append(.noSkolemInEquality(l: a, r: b, range: expr.range))
        return nil
      }

      return .equality(l: a, r: b)

    case .conformance(let l, let traits):
      guard let a = realize(name: l, inScope: scope) else { return nil }
      if !a.isTypeParam {
        diags.append(.noSkolemInConformance(a, range: expr.range))
        return nil
      }

      var b: Set<Type> = []
      for i in traits {
        guard let type = realize(name: i, inScope: scope) else { return nil }
        if type.base is TraitType {
          b.insert(type)
        } else {
          diags.append(.conformanceToNonTraitType(a, range: expr.range))
          return nil
        }
      }

      return .conformance(l: a, traits: b)

    case .size(let e):
      // TODO: Symbolic execution
      return .size(e)
    }
  }

  // MARK: Name binding

  /// The result of a name lookup.
  private typealias DeclSet = Set<AnyDeclID>

  /// A lookup table.
  private typealias LookupTable = [String: DeclSet]

  /// A set containing the extension declarations being currently bounded.
  ///
  /// This property is used during extension binding to avoid infinite recursion through qualified
  /// lookups into the extended type.
  private var extensionsUnderBinding: Set<NodeID<ExtensionDecl>> = []

  /// A table mapping a type to its lookup table.
  private var memberLookupTables: [Type: LookupTable] = [:]

  /// Returns the declarations that expose `name` without qualification in `scope`.
  private func lookup(unqualified name: String, inScope scope: AnyNodeID) -> DeclSet {
    assert(ast[scope] is LexicalScope)

    var scope = scope
    var matches: Set<AnyDeclID> = []

    while true {
      // Search for the name in the current scope.
      let newMatches = lookup(name, inScope: scope)

      // We can assume the matches are wither empty or all overloadable.
      matches.formUnion(newMatches)

      // We're done if we found at least one non-overloadable match.
      if newMatches.contains(where: { i in !(ast[i] is FunDecl) }) {
        return matches
      }

      // Move to the parent.
      if let parent = scopeHierarchy.parent[scope] {
        scope = parent
      } else if let std = ast.std, scope != std {
        scope = AnyNodeID(std)
      } else {
        break
      }
    }

    // Search for the name in the set of visible modules.
    if let i = ast.modules.first(where: { i in ast[i].name == name }) {
      matches.insert(AnyDeclID(i))
    }

    return matches
  }

  /// Returns the declarations that introduce `name` in `scope`.
  private func lookup(_ name: String, inScope scope: AnyNodeID) -> DeclSet {
    names(introducedIn: scope)[name, default: []]
  }

  /// Returns the declarations that introduce `name` as a member of `type`.
  private mutating func lookup(_ name: String, memberOf type: Type) -> DeclSet {
    if let table = memberLookupTables[type],
       let decls = table[name]
    {
      return decls
    }

    switch type {
    case .trait(let t):
      return lookup(name, inDeclSpaceOf: t.decl)
    default:
      unreachable("unexpected type")
    }
  }

  /// Returns the declarations that introduce `name` in the declaration space of `decl`.
  private mutating func lookup<T: DeclID>(_ name: String, inDeclSpaceOf decl: T) -> DeclSet {
    switch decl.kind {
    case .traitDecl:
      let decl = NodeID<TraitDecl>(converting: decl)!
      let type = Type.trait(TraitType(decl: decl, ast: ast))

      // Register direct members.
      memberLookupTables[type, default: [:]].merge(
        names(introducedIn: decl), uniquingKeysWith: { a, b in a.union(b) })

      // Register members declared in extensions.
      for i in extensions(of: type, exposedTo: scopeHierarchy.container[decl]!) {
        memberLookupTables[type, default: [:]].merge(
          names(introducedIn: i), uniquingKeysWith: { a, b in a.union(b) })
      }

      // TODO: Register members declared in conformances.
      // TODO: Register inherited members.

      return memberLookupTables[type, default: [:]][name, default: []]

    default:
      unreachable("unexpected declaration")
    }
  }

  /// Returns the extensions of `type` exposed to `scope`.
  private mutating func extensions(
    of type: Type,
    exposedTo scope: AnyNodeID
  ) -> [NodeID<ExtensionDecl>] {
    assert(ast[scope] is LexicalScope)

    var modules = ast.modules[0...]
    var matches: [NodeID<ExtensionDecl>] = []
    var scope = scope
    let canonicalType = canonicalize(type: type)

    // Look for extension declarations in all visible scopes.
    while true {
      let decls = scopeHierarchy.containees[scope, default: []]
      for i in decls where i.kind == .extensionDecl {
        // Skip extensions that are being bound.
        let candidate = NodeID<ExtensionDecl>(converting: i)!
        guard extensionsUnderBinding.insert(candidate).inserted else { continue }
        defer { extensionsUnderBinding.remove(candidate) }

        // Bind the extension's identifier.
        guard let subject = realize(ast[candidate].subject, inScope: scope) else { continue }
        if canonicalize(type: subject) == canonicalType {
          matches.append(candidate)
        }
      }

      // Move to the next scope.
      if let parent = scopeHierarchy.parent[scope], parent.kind != .moduleDecl {
        scope = parent
      } else if let module = modules.popFirst() {
        scope = AnyNodeID(module)
      } else {
        break
      }
    }

    return matches
  }

  /// Returns the names and declarations introduced in `scope`.
  private func names<T: NodeIDProtocol>(introducedIn scope: T) -> LookupTable {
    assert(ast[scope] is LexicalScope)
    guard let decls = scopeHierarchy.containees[scope] else { return [:] }

    var table: LookupTable = [:]
    for decl in decls {
      switch decl.kind {
      case .associatedSizeDecl,
           .associatedTypeDecl,
           .genericSizeParamDecl,
           .genericTypeParamDecl,
           .namespaceDecl,
           .paramDecl,
           .productTypeDecl,
           .traitDecl,
           .typeAliasDecl:
        let name = (ast[decl] as! SingleEntityDecl).name
        table[name, default: []].insert(AnyDeclID(decl))

      default:
        unreachable("unexpected declaration")
      }
    }

    // Note: Results should be memoized.
    return table
  }

  // MARK: Type realization

  /// The overarching type of each declaration.
  private var declTypes = DeclMap<Type?>()

  /// Realizes and returns the type denoted by `expr` evaluated in `scope`.
  private mutating func realize(
    _ expr: AnyTypeExprID,
    inScope scope: AnyNodeID
  ) -> Type? {
    assert(ast[scope] is LexicalScope)

    switch expr.base.kind {
    case .nameTypeExpr:
      return realize(name: NodeID(converting: expr)!, inScope: scope)
    case .tupleTypeExpr:
      return realize(tuple: NodeID(converting: expr)!, inScope: scope)
    default:
      unreachable("unexpected declaration")
    }
  }

  private mutating func realize(
    name i: NodeID<NameTypeExpr>,
    inScope scope: AnyNodeID
  ) -> Type? {
    assert(ast[scope] is LexicalScope)

    let identifier = ast[i].identifier
    var matches: DeclSet

    if let j = ast[i].domain {
      // Lookup for the name's identifier in the context of the domain.
      guard let domain = realize(j, inScope: scope) else { return nil }
      matches = lookup(identifier.value, memberOf: domain)

      // Filter out the value declarations.
      matches = matches.filter({ $0.kind <= .typeDecl })
      if matches.isEmpty {
        diags.append(.noType(named: identifier.value, in: domain, range: identifier.range))
        return nil
      }
    } else {
      // Bypass unqualified lookup if the name is a built-in alias.
      switch identifier.value {
      case "Any"  : return .any
      case "Never": return .never
      default:
        break
      }

      // Search for the referred type declaration with unqualified lookup.
      matches = lookup(unqualified: identifier.value, inScope: scope)

      // Filter out the value declarations.
      matches = matches.filter({ $0.kind <= .typeDecl  })
      if matches.isEmpty {
        diags.append(.noType(named: identifier.value, range: identifier.range))
        return nil
      }
    }

    if matches.count > 1 {
      diags.append(.ambiguousTypeReference(name: identifier.value, range: identifier.range))
      return nil
    }

    // Realize the referred type.
    guard let base = realize(decl: matches.first!) else { return nil }

    // Evaluate the arguments of the referred type, if any.
    if ast[i].arguments.isEmpty {
      return base
    } else {
      var arguments: [BoundGenericType.Argument] = []

      for a in ast[i].arguments {
        switch a {
        case .size(let a):
          // TODO: Symbolic execution
          arguments.append(.size(a))

        case .type(let a):
          guard let type = realize(a, inScope: scope) else { return nil }
          arguments.append(.type(type))
        }
      }

      return .boundGeneric(BoundGenericType(base, arguments: arguments))
    }
  }

  private mutating func realize(
    tuple i: NodeID<TupleTypeExpr>,
    inScope scope: AnyNodeID
  ) -> Type? {
    var elements: [TupleType.Element] = []
    elements.reserveCapacity(ast[i].elements.count)

    for e in ast[i].elements {
      guard let type = realize(e.value.type, inScope: scope) else { return nil }
      elements.append(TupleType.Element(label: e.value.label, type: type))
    }

    return .tuple(TupleType(elements))
  }

  /// Returns the overarching type of the specified declaration.
  private mutating func realize<T: DeclID>(decl i: T) -> Type? {
    switch i.kind {
    case .associatedSizeDecl,
         .genericSizeParamDecl:
      return _realize(decl: i, { (this, i) in
        .genericSizeParam(GenericSizeParamType(decl: i, ast: this.ast))
      })

    case .associatedTypeDecl,
         .genericTypeParamDecl:
      return _realize(decl: i, { (this, i) in
        .genericTypeParam(GenericTypeParamType(decl: i, ast: this.ast))
      })

    case .traitDecl:
      return _realize(decl: i, { (this, i) in
        .trait(TraitType(decl: NodeID(converting: i)!, ast: this.ast))
      })

    case .typeAliasDecl:
      return _realize(decl: i, { (this, i) in
        .typeAlias(TypeAliasType(decl: NodeID(converting: i)!, ast: this.ast))
      })

    default:
      unreachable("unexpected declaration")
    }
  }

  /// Returns the type of `decl` from the cache, or calls `action` to compute it and caches the
  /// result before returning it.
  private mutating func _realize<T: DeclID>(
    decl i: T,
    _ action: (inout Self, T) -> Type?
  ) -> Type? {
    // Check if a type realization request has already been received.
    switch declRequests[i] {
    case nil:
      declRequests[i] = .typeRealizationStarted

    case .typeRealizationStarted:
      diags.append(.circularDependency(range: ast.ranges[i]))
      return nil

    case .typeRealizationCompleted, .typeCheckingStarted, .success, .failure:
      return declTypes[i]!
    }

    // Process the request.
    declTypes[i] = action(&self, i)

    // Update the request status.
    declRequests[i] = .typeRealizationCompleted
    return declTypes[i]!
  }

}
