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

    /// Type checking failed.
    case failure

  }

  /// A cache for type checking requests on declarations.
  private var declRequests = DeclMap<RequestStatus>()

  /// A cache mapping declarations to the constraints extracted from their generic clauses.
  private var clauseConstraints = DeclMap<[TypeConstraint]?>()

  /// Type checks the specified module and returns whether that succeeded.
  ///
  /// - Requires: `module` is a valid index in the type checker's AST.
  public mutating func check(module: NodeIndex<ModuleDecl>) -> Bool {
    // Build the type of the module.
    declTypes[AnyDeclIndex(module)] = .module(ModuleType(decl: module))

    // Type check the declarations in the module.
    return ast[module].members.reduce(true, { (success, i) in
      check(decl: i) && success
    })
  }

  /// Type checkes the specified declaration and returns whether that succeeded.
  private mutating func check(decl: AnyDeclIndex) -> Bool {
    // Check if a type checking request has already been received.
    while true {
      switch declRequests[decl] {
      case nil:
        // Realize the type of the declaration before starting type checking.
        _  = realize(typeOf: decl)
        assert(declRequests[decl] != nil)
        continue

      case .typeRealizationCompleted:
        declRequests[decl] = .typeCheckingStarted

      case .typeRealizationStarted, .typeCheckingStarted:
        diags.append(.circularDependency(range: ast.ranges[decl]))
        return false

      case .success:
        return true

      case .failure:
        return false
      }

      break
    }

    // Process the request.
    let success: Bool
    switch decl.base.kind {
    case .associatedSizeDecl:
      success = check(associatedSize: decl.convert(to: AssociatedSizeDecl.self)!)
    case .associatedTypeDecl:
      success = check(associatedType: decl.convert(to: AssociatedTypeDecl.self)!)
    case .traitDecl:
      success = check(trait: decl.convert(to: TraitDecl.self)!)
    default:
      unreachable("unexpected declaration type")
    }

    // Update the request status.
    declRequests[decl] = success ? .success : .failure
    return success
  }

  private mutating func check(associatedSize: NodeIndex<AssociatedSizeDecl>) -> Bool {
    return true
  }

  private mutating func check(associatedType: NodeIndex<AssociatedTypeDecl>) -> Bool {
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

  private mutating func check(trait i: NodeIndex<TraitDecl>) -> Bool {
    // Type check the generic constraints of the declaration.
    var success = constraints(ofTrait: i) != nil

    // Type check the type's direct members.
    for j in ast[i].members {
      success = check(decl: j) && success
    }

    // Type check extensions.
    let type = Type.trait(TraitType(decl: i))
    for j in extensions(of: type, exposedTo: scopeHierarchy.container[i]!) {
      success = check(decl: AnyDeclIndex(j)) && success
    }

    // TODO: Check the type's conformance

    return success
  }

  private mutating func check(typeAlias: TypeAliasDecl) -> Bool {
    fatalError("not implemented")
  }

  private mutating func check(`var`: VarDecl) -> Bool {
    fatalError("not implemented")
  }

  /// Returns the constraints on the generic parameters of defined by `decl`'s generic clause, or
  /// `nil` if those constraints are ill-formed.
  ///
  /// - Note: Call `constraints(ofTrait)` instead of this method to collect the constraints of a
  ///   trait declaration.
  private mutating func constraints<T: GenericDecl>(of decl: NodeIndex<T>) -> [TypeConstraint]? {
    assert(T.self != TraitDecl.self)
    if let constraints = clauseConstraints[AnyDeclIndex(decl)] {
      return constraints
    }

    guard ast[decl].genericClause != nil else {
      clauseConstraints[AnyDeclIndex(decl)] = []
      return []
    }

    fatalError("not implemented")
  }

  /// Returns the constraints on the types conforming to `decl` and its associated types, or `nil`
  /// if those constraints are ill-formed.
  private mutating func constraints(ofTrait i: NodeIndex<TraitDecl>) -> [TypeConstraint]? {
    if let constraints = clauseConstraints[AnyDeclIndex(i)] {
      return constraints
    }

    var success = true
    var constraints: [TypeConstraint] = []

    // Collect and type check the constraints defined on associated types and sizes.
    for member in ast[i].members {
      switch member.kind {
      case .associatedSizeDecl:
        success = associatedConstraints(
          ofSize: member.convert(to: AssociatedSizeDecl.self)!,
          ofTrait: i,
          into: &constraints) && success

      case .associatedTypeDecl:
        success = associatedConstraints(
          ofType: member.convert(to: AssociatedTypeDecl.self)!,
          ofTrait: i,
          into: &constraints) && success

      default:
        continue
      }
    }

    // Bail out if we found ill-form constraints.
    if !success {
      clauseConstraints[AnyDeclIndex(i)] = nil
      return nil
    }

    // Synthesize `Self: T`.
    let trait = TraitType(decl: i)
    let selfType = GenericTypeParamType(decl: AnyDeclIndex(i))
    constraints.append(.conformance(l: .genericTypeParam(selfType), traits: [.trait(trait)]))

    clauseConstraints[AnyDeclIndex(i)] = constraints
    return constraints
  }

  // Evaluates the constraints declared in `associatedSize`, stores them in `constraints` and
  // returns whether they are all well-formed.
  private mutating func associatedConstraints(
    ofSize associatedSize: NodeIndex<AssociatedSizeDecl>,
    ofTrait trait: NodeIndex<TraitDecl>,
    into constraints: inout [TypeConstraint]
  ) -> Bool {
    // Realize the generic type parameter corresponding to the associated size.
    guard let lhs = realize(typeOf: AnyDeclIndex(associatedSize)) else { return false }
    assert(lhs.base is GenericSizeParamType)

    var success = true

    // Evaluate the constraint expressions of the associated type's where clause.
    if let clause = ast[associatedSize].whereClause?.value {
      for expr in clause.constraints {
        if let constraint = eval(constraintExpr: expr, in: AnyNodeIndex(trait)) {
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
    ofType associatedType: NodeIndex<AssociatedTypeDecl>,
    ofTrait trait: NodeIndex<TraitDecl>,
    into constraints: inout [TypeConstraint]
  ) -> Bool {
    // Realize the generic type parameter corresponding to the associated type.
    guard let lhs = realize(typeOf: AnyDeclIndex(associatedType)) else { return false }
    assert(lhs.base is GenericTypeParamType)

    var success = true

    // Realize the traits in the conformance list.
    var traits: Set<Type> = []
    for expr in ast[associatedType].conformances {
      guard let rhs = realize(name: expr, in: AnyNodeIndex(trait)) else {
        success = false
        continue
      }

      /// RHS must be a trait.
      if rhs.base is TraitType {
        traits.insert(rhs)
      } else {
        diags.append(.conformanceToNonTraitType(rhs.describe(in: ast), range: ast.ranges[expr]))
        success = false
      }
    }

    // Create a constraint for the traits in the conformance list.
    if !traits.isEmpty {
      constraints.append(.conformance(l: lhs, traits: traits))
    }

    // Evaluate the constraint expressions of the associated type's where clause.
    if let clause = ast[associatedType].whereClause?.value {
      for expr in clause.constraints {
        if let constraint = eval(constraintExpr: expr, in: AnyNodeIndex(trait)) {
          constraints.append(constraint)
        } else {
          success = false
        }
      }
    }

    return success
  }

  /// Evaluates `expr` in `scope` and returns a type constraint, or `nil` if evaluation failed.
  ///
  /// - Note: Calling this method multiple times with the same arguments may duplicate diagnostics.
  private mutating func eval(
    constraintExpr expr: SourceRepresentable<WhereClause.ConstraintExpr>,
    in scope: AnyNodeIndex
  ) -> TypeConstraint? {
    assert(ast[scope] is LexicalScope)

    switch expr.value {
    case .equality(let l, let r):
      guard let a = realize(l, in: scope) else { return nil }
      guard let b = realize(r, in: scope) else { return nil }

      if !a.isTypeParam && !b.isTypeParam {
        diags.append(.noSkolemInEquality(
          l: a.describe(in: ast), r: b.describe(in: ast), range: expr.range))
        return nil
      }

      return .equality(l: a, r: b)

    case .conformance(let l, let traits):
      guard let a = realize(AnyTypeExprIndex(l), in: scope) else { return nil }
      if !a.isTypeParam {
        diags.append(.noSkolemInConformance(a.describe(in: ast), range: expr.range))
        return nil
      }

      var b: Set<Type> = []
      for i in traits {
        guard let type = realize(AnyTypeExprIndex(i), in: scope) else { return nil }
        if type.base is TraitType {
          b.insert(type)
        } else {
          diags.append(.conformanceToNonTraitType(a.describe(in: ast), range: expr.range))
          return nil
        }
      }

      return .conformance(l: a, traits: b)

    case .size(let e):
      // TODO: Symbolic execution
      return .size(e)
    }
  }


  func g<U>(u: U) {
    func f<T>(x: T) where U: Equatable & Equatable {

    }
  }

  // MARK: Name binding

  /// The result of a name lookup.
  private typealias DeclSet = Set<AnyDeclIndex>

  /// A set containing the extension declarations being currently bounded.
  ///
  /// This property is used during extension binding to avoid infinite recursion through qualified
  /// lookups into the extended type.
  private var extensionsUnderBinding: Set<NodeIndex<ExtensionDecl>> = []

  /// Searches and returns the declarations that introduce `name`, unqualified from `scope`.
  private func lookup(
    _ name: String,
    unqualifiedFrom scope: AnyNodeIndex
  ) -> DeclSet {
    assert(ast[scope] is LexicalScope)

    var scope = scope
    var matches: Set<AnyDeclIndex> = []

    while true {
      // Search for the name in the current scope.
      let newMatches = lookup(name, qualifiedBy: scope)

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
        scope = AnyNodeIndex(std)
      } else {
        break
      }
    }

    // Search for the name in the set of visible modules.
    if let i = ast.modules.first(where: { i in ast[i].name == name }) {
      matches.insert(AnyDeclIndex(i))
    }

    return matches
  }

  /// Returns the declarations that introduce `name`, qualified by `scope`.
  private func lookup(
    _ name: String,
    qualifiedBy scope: AnyNodeIndex
  ) -> DeclSet {
    assert(ast[scope] is LexicalScope)
    guard let decls = scopeHierarchy.containees[scope] else { return [] }

    // Note: Linear search could be optimized with a lookup table.
    var matches: DeclSet = []
    for decl in decls {
      for (n, d) in expand(decl, in: ast) {
        if n == name {
          matches.insert(d)
        }
      }
    }
    return matches
  }

  /// Returns the declaration of the names introdced by `decl`.
  private func expand<T: DeclIndex>(_ decl: T, in ast: AST) -> [(String, AnyDeclIndex)] {
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
      return [(name, AnyDeclIndex(decl))]

    default:
      unreachable("unexpected declaration")
    }
  }

  /// Returns the extensions of `type` exposed to `scope`.
  private mutating func extensions(
    of type: Type,
    exposedTo scope: AnyNodeIndex
  ) -> [NodeIndex<ExtensionDecl>] {
    assert(ast[scope] is LexicalScope)

    var modules = ast.modules[1...]
    var matches: [NodeIndex<ExtensionDecl>] = []
    var scope = scope
    let type = type.canonical()

    // Look for extension declarations in all visible scopes.
    while true {
      let decls = scopeHierarchy.containees[scope, default: []]
      for i in decls where i.kind == .extensionDecl {
        // Skip extensions that are being bound.
        let candidate = i.convert(to: ExtensionDecl.self)!
        guard extensionsUnderBinding.insert(candidate).inserted else { continue }
        defer { extensionsUnderBinding.remove(candidate) }

        // Bind the extension's identifier.
        guard let subject = realize(ast[candidate].subject, in: scope) else { continue }
        if subject ~ type {
          matches.append(candidate)
        }
      }

      // Move to the next scope.
      if let parent = scopeHierarchy.parent[scope], parent.kind != .moduleDecl {
        scope = parent
      } else if let module = modules.popFirst() {
        scope = AnyNodeIndex(module)
      } else {
        break
      }
    }

    return matches
  }

  // MARK: Type realization

  /// The overarching type of each declaration.
  private var declTypes = DeclMap<Type?>()

  /// The realized types of the type expressions.
  private var typeExprTypes = NodeMap<Type?>()

  /// Realizes and returns the type denoted by `expr` evaluated in `scope`.
  private mutating func realize(
    _ expr: AnyTypeExprIndex,
    in scope: AnyNodeIndex
  ) -> Type? {
    assert(ast[scope] is LexicalScope)
    if let type = typeExprTypes[expr] {
      return type
    }

    // Process the request.
    switch expr.base.kind {
    case .nameTypeExpr:
      typeExprTypes[expr] = realize(name: expr.convert(to: NameTypeExpr.self)!, in: scope)

    default:
      unreachable("unexpected declaration type")
    }

    return typeExprTypes[expr]!
  }

  private mutating func realize(
    name expr: NodeIndex<NameTypeExpr>,
    in scope: AnyNodeIndex
  ) -> Type? {
    assert(ast[scope] is LexicalScope)

    if ast[expr].domain == nil {
      // Bypass unqualified lookup if the name is a built-in alias.
      let identifier = ast[expr].identifier
      switch identifier.value {
      case "Any"  : return .any
      case "Never": return .never
      default:
        break
      }

      // Search for the referred type declaration.
      var matches = lookup(identifier.value, unqualifiedFrom: AnyNodeIndex(scope))

      // Filter out the value declarations.
      matches = matches.filter({ $0.kind <= .typeDecl  })

      if matches.isEmpty {
        diags.append(.noType(named: identifier.value, range: identifier.range))
        return nil
      } else if matches.count > 1 {
        diags.append(.ambiguousTypeReference(name: identifier.value, range: identifier.range))
        return nil
      } else {
        return realize(typeOf: matches.first!)
      }
    }

    fatalError("not implemented")
  }

  /// Returns the overarching type of the specified declaration.
  private mutating func realize(typeOf decl: AnyDeclIndex) -> Type? {
    // Check if a type realization request has already been received.
    switch declRequests[decl] {
    case nil:
      declRequests[decl] = .typeRealizationStarted

    case .typeRealizationStarted:
      diags.append(.circularDependency(range: ast.ranges[decl]))
      return nil

    case .typeRealizationCompleted, .typeCheckingStarted, .success, .failure:
      return declTypes[decl]!
    }

    // Process the request.
    switch decl.base.kind {
    case .associatedSizeDecl:
      declTypes[decl] = .genericSizeParam(GenericSizeParamType(decl: decl))

    case .associatedTypeDecl:
      declTypes[decl] = .genericTypeParam(GenericTypeParamType(decl: decl))

    case .traitDecl:
      declTypes[decl] = .trait(TraitType(decl: decl.convert(to: TraitDecl.self)!))

    default:
      unreachable("unexpected declaration type")
    }

    // Update the request status.
    declRequests[decl] = .typeRealizationCompleted
    return declTypes[decl]!
  }

}
