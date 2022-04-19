import Utils

/// Val's type checker.
public struct TypeChecker {

  /// The AST containing the modules being type checked.
  var ast: AST

  /// The scope hierarchy of the AST.
  let scopeHierarchy: ScopeHierarchy

  /// The diagnostics of the type errors.
  public private(set) var diagnostics: Set<Diagnostic> = []

  /// The overarching type of each declaration.
  public private(set) var declTypes = DeclMap<Type?>()

  /// The type of each expression.
  public internal(set) var exprTypes = ExprMap<Type>()

  /// Creates a new type checker for the specified AST.
  ///
  /// - Note: `ast` is stored in the type checker and mutated throughout type checking (e.g., to
  ///   insert synthesized declarations).
  public init(ast: AST) {
    self.ast = ast
    self.scopeHierarchy = ast.scopeHierarchy()
  }

  /// Calls `action` with a mutable projection of `self`, which returns a pair `(a, b)`. `self` is
  /// reassigned to `a` and `b` is returned.
  private mutating func withBorrowedSelf<T>(_ action: (Self) -> (Self, T)) -> T {
    withUnsafeMutablePointer(to: &self, { this in
      let (a, b) = action(this.move())
      this.initialize(to: a)
      return b
    })
  }

  // MARK: Type system

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
  public func canonicalize(constraint: Constraint) -> Constraint {
    var canonical = constraint
    canonical.visitTypes({ $0 = canonicalize(type: $0) })
    return canonical
  }

  /// Returns the set of traits to which `type` conforms in `scope`.
  ///
  /// - Note: If `type` is a trait, it is always contained in the returned set.
  mutating func conformedTraits(of type: Type, inScope scope: AnyNodeID) -> Set<TraitType>? {
    // TODO: Must be fallible!
    assert(scope.kind <= .lexicalScope)
    var result: Set<TraitType> = []

    switch type {
    case .genericTypeParam(let t):
      // Gather the conformances defined at declaration.
      switch t.decl.kind {
      case .genericTypeParamDecl:
        guard let traits = realize(
          conformances: ast[NodeID<GenericTypeParamDecl>(converting: t.decl)!].conformances,
          inScope: scopeHierarchy.parent[t.decl]!)
        else { return nil }
        result.formUnion(traits)

      case .traitDecl:
        let trait = TraitType(decl: NodeID(converting: t.decl)!, ast: ast)
        return conformedTraits(of: .trait(trait), inScope: scope)

      default:
        break
      }

      // Gather conformances defined by conditional conformances/extensions.
      for scope in scopeHierarchy.scopesToRoot(from: scope) where scope.kind <= .genericScope {
        guard let e = environment(of: scope) else { continue }
        result.formUnion(e.conformedTraits(of: type))
      }

    case .product:
      fatalError("not implemented")

    case .trait(let t):
      // Gather the conformances defined at declaration.
      guard var work = realize(
        conformances: ast[t.decl].refinements,
        inScope: scopeHierarchy.parent[t.decl]!)
      else { return nil }

      while let base = work.popFirst() {
        if base == t {
          diagnostics.insert(.circularRefinement(range: ast[t.decl].identifier.range))
          return nil
        } else if result.insert(base).inserted {
          guard let traits = realize(
            conformances: ast[base.decl].refinements,
            inScope: scopeHierarchy.parent[base.decl]!)
          else { return nil }
          work.formUnion(traits)
        }
      }

      // Add the trait to its own conformance set.
      result.insert(t)

      // Traits can't be refined in extensions; we're done.
      return result

    case .typeAlias:
      break

    default:
      break
    }

    // Collect traits declared in conformance declarations.
    for i in extendingDecls(of: type, exposedTo: scope) where i.kind == .conformanceDecl {
      let decl = ast[NodeID<ConformanceDecl>(converting: i)!]
      let declScope = scopeHierarchy.container[i]!
      guard let traits = realize(conformances: decl.conformances, inScope: declScope)
        else { return nil }

      for trait in traits {
        guard let bases = conformedTraits(of: .trait(trait), inScope: declScope)
          else { return nil }
        result.formUnion(bases)
      }
    }

    return result
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

  /// A cache mapping generic declarations to their environment.
  private var environments = DeclMap<MemoizationState<GenericEnvironment?>>()

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
    case .bindingDecl:
      return check(binding: NodeID(converting: i)!)
    case .productTypeDecl:
      return check(productType: NodeID(converting: i)!)
    case .subscriptDecl:
      return check(subscript: NodeID(converting: i)!)
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

  private mutating func check(binding i: NodeID<BindingDecl>) -> Bool {
    // Note: binding declarations do not undergo type realization.
    switch declRequests[i] {
    case nil:
      declRequests[i] = .typeCheckingStarted
    case .typeCheckingStarted:
      diagnostics.insert(.circularDependency(range: ast.ranges[i]))
      return false
    case .success:
      return true
    case .failure:
      return false
    default:
      unreachable()
    }

    let scope = scopeHierarchy.container[AnyDeclID(i)]!
    let pattern = ast[i].pattern
    var constraints: [LocatableConstraint] = []
    guard let type = infer(
      pattern: pattern, expectedType: nil, inScope: scope, constraints: &constraints)
    else {
      declTypes[i] = nil
      declRequests[i] = .failure
      return false
    }

    // Type check the initializer, if any, optionally completing partially realized parts.
    var success = true
    if var initializer = ast[i].initializer {
      (success, _) = infer(
        expr: &initializer,
        expectedType: type,
        inScope: scope,
        constraints: &constraints)
      ast[i].initializer = initializer
    }

    // TODO: assign variable declarations to their type

    if success {
      declTypes[i] = type
      declRequests[i] = .success
      return true
    } else {
      declTypes[i] = nil
      declRequests[i] = .failure
      return true
    }
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

  private mutating func check(productType i: NodeID<ProductTypeDecl>) -> Bool {
    _check(decl: i, { (this, i) in
      // Type check the generic constraints of the declaration.
      var success = this.environment(ofGenericDecl: i) != nil

      // Type check the type's direct members.
      for j in this.ast[i].members {
        success = this.check(decl: j) && success
      }

      // Type check extending declarations.
      let type = this.declTypes[i]!!
      for j in this.extendingDecls(of: type, exposedTo: this.scopeHierarchy.container[i]!) {
        success = this.check(decl: j) && success
      }

      // TODO: Check the conformances

      return success
    })
  }

  private mutating func check(subscript i: NodeID<SubscriptDecl>) -> Bool {
    _check(decl: i, { (this, i) in
      // TODO: Implement me
      true
    })
  }

  private mutating func check(subscriptImpl: SubscriptImplDecl) -> Bool {
    fatalError("not implemented")
  }

  private mutating func check(trait i: NodeID<TraitDecl>) -> Bool {
    _check(decl: i, { (this, i) in
      // Type check the generic constraints of the declaration.
      var success = this.environment(ofTraitDecl: i) != nil

      // Type check the type's direct members.
      for j in this.ast[i].members {
        success = this.check(decl: j) && success
      }

      // Type check extending declarations.
      let type = this.declTypes[i]!!
      for j in this.extendingDecls(of: type, exposedTo: this.scopeHierarchy.container[i]!) {
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
      var success = this.environment(ofGenericDecl: i) != nil

      // Type check extending declarations.
      for j in this.extendingDecls(of: subject, exposedTo: this.scopeHierarchy.container[i]!) {
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
        /// The the overarching type of the declaration is available after type realization.
        defer { assert(declRequests[i] != nil) }

        // Realize the type of the declaration before starting type checking.
        if realize(decl: i) != nil {
          // Note: Because type realization might perform type checking, we should re-check the
          // status of the request.
          continue
        } else {
          // Type checking fails if type realization did.
          declRequests[i] = .failure
          return false
        }

      case .typeRealizationCompleted:
        declRequests[i] = .typeCheckingStarted

      case .typeRealizationStarted, .typeCheckingStarted:
        // Note: The request status will be updated when the request that caused the circular
        // dependency handles the failure.
        diagnostics.insert(.circularDependency(range: ast.ranges[i]))
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

  /// Returns the generic environment defined by `i`, or `nil` if it is ill-formed.
  ///
  /// - Requires: `i.kind <= .genericScope`
  private mutating func environment<T: NodeIDProtocol>(of i: T) -> GenericEnvironment? {
    switch i.kind {
    case .funDecl:
      return environment(ofGenericDecl: NodeID<FunDecl>(converting: i)!)
    case .productTypeDecl:
      return environment(ofGenericDecl: NodeID<ProductTypeDecl>(converting: i)!)
    case .subscriptDecl:
      return environment(ofGenericDecl: NodeID<SubscriptDecl>(converting: i)!)
    case .typeAliasDecl:
      return environment(ofGenericDecl: NodeID<TypeAliasDecl>(converting: i)!)
    case .traitDecl:
      return environment(ofTraitDecl: NodeID(converting: i)!)
    default:
      unreachable("unexpected scope")
    }
  }

  /// Returns the generic environment defined by `i`, or `nil` if it is ill-formed.
  private mutating func environment<T: GenericDecl>(
    ofGenericDecl i: NodeID<T>
  ) -> GenericEnvironment? {
    switch environments[i] {
    case .done(let e):
      return e
    case .inProgress:
      fatalError("circular dependency")
    case nil:
      environments[i] = .inProgress
    }

    // Nothing to do if the declaration has no generic clause.
    guard let clause = ast[i].genericClause?.value else {
      let e = GenericEnvironment(decl: i, constraints: [], into: &self)
      environments[i] = .done(e)
      return e
    }

    let declScope = AnyNodeID(i)
    let parentScope = scopeHierarchy.parent[declScope]!
    var success = true
    var constraints: [Constraint] = []

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
      guard let traits = realize(conformances: list, inScope: parentScope) else { return nil }
      if !traits.isEmpty {
        constraints.append(.conformance(l: lhs, traits: traits))
      }
    }

    // Evaluate the constraint expressions of the associated type's where clause.
    if let whereClause = clause.whereClause?.value {
      for expr in whereClause.constraints {
        if let constraint = eval(constraintExpr: expr, inScope: declScope) {
          constraints.append(constraint)
        } else {
          success = false
        }
      }
    }

    if success {
      let e = GenericEnvironment(decl: i, constraints: constraints, into: &self)
      environments[i] = .done(e)
      return e
    } else {
      environments[i] = .done(nil)
      return nil
    }
  }

  /// Returns the generic environment defined by `i`, or `nil` if it is ill-formed.
  private mutating func environment<T: TypeExtendingDecl>(
    ofTypeExtendingDecl i: NodeID<T>
  ) -> GenericEnvironment? {
    switch environments[i] {
    case .done(let e):
      return e
    case .inProgress:
      fatalError("circular dependency")
    case nil:
      environments[i] = .inProgress
    }

    let scope = AnyNodeID(i)
    var success = true
    var constraints: [Constraint] = []

    // Evaluate the constraint expressions of the associated type's where clause.
    if let whereClause = ast[i].whereClause?.value {
      for expr in whereClause.constraints {
        if let constraint = eval(constraintExpr: expr, inScope: scope) {
          constraints.append(constraint)
        } else {
          success = false
        }
      }
    }

    if success {
      let e = GenericEnvironment(decl: i, constraints: constraints, into: &self)
      environments[i] = .done(e)
      return e
    } else {
      environments[i] = .done(nil)
      return nil
    }
  }

  /// Returns the generic environment defined by `i`, or `nil` if it is ill-formed.
  private mutating func environment(
    ofTraitDecl i: NodeID<TraitDecl>
  ) -> GenericEnvironment? {
    switch environments[i] {
    case .done(let e):
      return e
    case .inProgress:
      fatalError("circular dependency")
    case nil:
      environments[i] = .inProgress
    }

    var success = true
    var constraints: [Constraint] = []

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
      environments[i] = .done(nil)
      return nil
    }

    // Synthesize `Self: T`.
    let selfType = GenericTypeParamType(decl: i, ast: ast)
    guard case .trait(let trait) = declTypes[i]!! else { unreachable() }
    constraints.append(.conformance(l: .genericTypeParam(selfType), traits: [trait]))

    let e = GenericEnvironment(decl: i, constraints: constraints, into: &self)
    environments[i] = .done(e)
    return e
  }

  // Evaluates the constraints declared in `associatedSize`, stores them in `constraints` and
  // returns whether they are all well-formed.
  private mutating func associatedConstraints(
    ofSize associatedSize: NodeID<AssociatedSizeDecl>,
    ofTrait trait: NodeID<TraitDecl>,
    into constraints: inout [Constraint]
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
    into constraints: inout [Constraint]
  ) -> Bool {
    // Realize the generic type parameter corresponding to the associated type.
    guard let lhs = realize(decl: associatedType) else { return false }
    assert(lhs.base is GenericTypeParamType)

    // Synthesize the sugared conformance constraint, if any.
    let list = ast[associatedType].conformances
    guard let traits = realize(conformances: list, inScope: AnyNodeID(trait)) else { return false }
    if !traits.isEmpty {
      constraints.append(.conformance(l: lhs, traits: traits))
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

  /// Evaluates `expr` in `scope` and returns a type constraint, or `nil` if evaluation failed.
  ///
  /// - Note: Calling this method multiple times with the same arguments may duplicate diagnostics.
  private mutating func eval(
    constraintExpr expr: SourceRepresentable<WhereClause.ConstraintExpr>,
    inScope scope: AnyNodeID
  ) -> Constraint? {
    assert(scope.kind <= .lexicalScope)

    switch expr.value {
    case .equality(let l, let r):
      guard let a = realize(l, inScope: scope) else { return nil }
      guard let b = realize(r, inScope: scope) else { return nil }

      if !a.isTypeParam && !b.isTypeParam {
        diagnostics.insert(.noSkolemInEquality(l: a, r: b, range: expr.range))
        return nil
      }

      return .equality(l: a, r: b)

    case .conformance(let l, let traits):
      guard let a = realize(name: l, inScope: scope) else { return nil }
      if !a.isTypeParam {
        diagnostics.insert(.noSkolemInConformance(a, range: expr.range))
        return nil
      }

      var b: Set<TraitType> = []
      for i in traits {
        guard let type = realize(name: i, inScope: scope) else { return nil }
        if case .trait(let trait) = type {
          b.insert(trait)
        } else {
          diagnostics.insert(.conformanceToNonTraitType(a, range: expr.range))
          return nil
        }
      }

      return .conformance(l: a, traits: b)

    case .size(let e):
      // TODO: Symbolic execution
      return .size(e)
    }
  }

  // MARK: Type inference

  /// Infers the type of `expr`.
  private mutating func infer(
    expr: inout AnyExprID,
    expectedType: Type?,
    inScope scope: AnyNodeID,
    constraints: inout [LocatableConstraint]
  ) -> (success: Bool, solution: Solution) {
    assert(scope.kind <= .lexicalScope)

    exprTypes[expr] = expectedType ?? .variable(TypeVariable(node: AnyNodeID(expr)))

    // Temporarily projects `self` into a constraint generator.
    expr = withBorrowedSelf({ (this) -> (TypeChecker, AnyExprID) in
      var visitor = ConstraintGenerator(checker: this)
      let newExpr = expr.accept(&visitor)
      constraints.append(contentsOf: visitor.constraints)
      return (visitor.checker.release(), newExpr)
    })

    // Solve the constraints.
    var solver = ConstraintSolver(fresh: constraints)
    let solution = solver.solve()

    return (success: solution.errors.isEmpty, solution: solution)
  }

  /// Infers the type of `pattern`, generating the type constraints implied by the expressions it
  /// may contain.
  ///
  /// - Note: A `nil` return signals a failure to infer the type of the pattern.
  private mutating func infer<T: PatternID>(
    pattern: T,
    expectedType: Type?,
    inScope scope: AnyNodeID,
    constraints: inout [LocatableConstraint]
  ) -> Type? {
    assert(scope.kind <= .lexicalScope)

    switch pattern.kind {
    case .bindingPattern:
      // A binding pattern introduces additional type information when it has a type annotation. In
      // that case, the type denoted by the annotation is used to infer the type of the sub-pattern
      // and constrained to be a subtype of the expected type, if any.
      let lhs = ast[NodeID<BindingPattern>(converting: pattern)!]
      var subpatternType = expectedType
      if let annotation = lhs.annotation {
        if let type = realize(annotation, inScope: scope) {
          if let r = expectedType {
            constraints.append(LocatableConstraint(
              .subtyping(l: type, r: r), node: AnyNodeID(pattern), cause: .annotation))
          }
          subpatternType = type
        } else {
          return nil
        }
      }

      return infer(
        pattern: lhs.subpattern,
        expectedType: subpatternType,
        inScope: scope,
        constraints: &constraints)

    case .expr:
      fatalError("not implemented")

    case .namePattern, .wildcardPattern:
      return expectedType ?? .variable(TypeVariable())

    case .tuplePattern:
      let lhs = ast[NodeID<TuplePattern>(converting: pattern)!]
      switch expectedType {
      case .tuple(let rhs):
        // The pattern and the expected have a tuple shape.
        if rhs.elements.count == lhs.elements.count {
          var lLabels: [String?] = []
          var rLabels: [String?] = []

          // Visit the elements pairwise.
          for (a, b) in zip(lhs.elements, rhs.elements) {
            if infer(
              pattern: a.value.pattern,
              expectedType: b.type,
              inScope: scope,
              constraints: &constraints) == nil
            { return nil }

            lLabels.append(a.value.label)
            rLabels.append(b.label)
          }

          // Check that labels match.
          if lLabels == rLabels {
            return expectedType
          } else {
            diagnostics.insert(.incompatibleLabels(lLabels, rLabels, range: ast.ranges[pattern]))
            return nil
          }
        } else {
          // Invalid destructuring.
          diagnostics.insert(.invalidDestructuring(
            ofType: expectedType!, range: ast.ranges[pattern]))
          return nil
        }

      case .some:
        // The pattern has a tuple shape, the expected type hasn't.
        diagnostics.insert(.invalidDestructuring(
          ofType: expectedType!, range: ast.ranges[pattern]))
        return nil

      case nil:
        // Infer the shape of the expected type.
        var elements: [TupleType.Element] = []
        for element in lhs.elements {
          guard let type = infer(
            pattern: element.value.pattern,
            expectedType: nil,
            inScope: scope,
            constraints: &constraints)
          else { return nil }
          elements.append(TupleType.Element(label: element.value.label, type: type))
        }
        return .tuple(TupleType(elements))
      }

    default:
      unreachable("unexpected pattern")
    }
  }

  // MARK: Name binding

  /// The result of a name lookup.
  private typealias DeclSet = Set<AnyDeclID>

  /// A lookup table.
  private typealias LookupTable = [String: DeclSet]

  private struct MemberLookupKey: Hashable {

    var type: Type

    var scope: AnyNodeID

  }

  /// The member lookup tables of the types.
  ///
  /// This property is used to memoize the results of `lookup(_:memberOf:inScope)`.
  private var memberLookupTables: [MemberLookupKey: LookupTable] = [:]

  /// A set containing the type extending declarations being currently bounded.
  ///
  /// This property is used during conformance and extension binding to avoid infinite recursion
  /// through qualified lookups into the extended type.
  private var extensionsUnderBinding = DeclSet()

  /// Returns the declarations that expose `name` without qualification in `scope`.
  private mutating func lookup(unqualified name: String, inScope scope: AnyNodeID) -> DeclSet {
    assert(scope.kind <= .lexicalScope)

    let origin = scope
    var root = scope

    var matches = DeclSet()
    for scope in scopeHierarchy.scopesToRoot(from: scope) {
      // Search for the name in the current scope.
      let newMatches = lookup(name, inDeclSpaceOf: scope, exposedTo: origin)

      // We can assume the matches are either empty or all overloadable.
      matches.formUnion(newMatches)

      // We're done if we found at least one non-overloadable match.
      if newMatches.contains(where: { i in !(ast[i] is FunDecl) }) {
        return matches
      }

      root = scope
    }

    // We're done if we found at least one match.
    if !matches.isEmpty { return matches }

    // Check if the name refers to the module containing `scope`.
    if let module = NodeID<ModuleDecl>(converting: root), ast[module].name == name {
      return [AnyDeclID(module)]
    }

    // Search for the name in imported modules.
    for module in ast.modules where module != root {
      matches.formUnion(names(introducedIn: module)[name, default: []])
    }

    return matches
  }

  /// Returns the declarations that introduce `name` in the declaration space of `scope`.
  private mutating func lookup<T: NodeIDProtocol>(
    _ name: String,
    inDeclSpaceOf scope: T,
    exposedTo origin: AnyNodeID
  ) -> DeclSet {
    switch scope.kind {
    case .productTypeDecl:
      let type = Type.product(ProductType(decl: NodeID(converting: scope)!, ast: ast))
      return lookup(name, memberOf: type, inScope: origin)

    case .traitDecl:
      let type = Type.trait(TraitType(decl: NodeID(converting: scope)!, ast: ast))
      return lookup(name, memberOf: type, inScope: origin)

    case .typeAliasDecl:
      let type = Type.typeAlias(TypeAliasType(decl: NodeID(converting: scope)!, ast: ast))
      return lookup(name, memberOf: type, inScope: origin)

    default:
      return names(introducedIn: scope)[name, default: []]
    }
  }

  /// Returns the declarations that introduce `name` as a member of `type` in `scope`.
  private mutating func lookup(
    _ name: String,
    memberOf type: Type,
    inScope scope: AnyNodeID
  ) -> DeclSet {
    if case .conformanceLens(let t) = type {
      return lookup(name, memberOf: .trait(t.focus), inScope: scope)
    }

    let key = MemberLookupKey(type: type, scope: scope)
    if let m = memberLookupTables[key]?[name] {
      return m
    }

    var matches: DeclSet
    defer { memberLookupTables[key, default: [:]][name] = matches }

    switch type {
    case .product(let t):
      matches = names(introducedIn: t.decl)[name, default: []]
    case .trait(let t):
      matches = names(introducedIn: t.decl)[name, default: []]
    case .typeAlias(let t):
      matches = names(introducedIn: t.decl)[name, default: []]
    default:
      matches = DeclSet()
    }

    // We're done if we found at least one non-overloadable match.
    if matches.contains(where: { i in !(ast[i] is FunDecl) }) {
      return matches
    }

    // Look for members declared in extensions.
    for i in extendingDecls(of: type, exposedTo: scope) {
      matches.formUnion(names(introducedIn: i)[name, default: []])
    }

    // We're done if we found at least one non-overloadable match.
    if matches.contains(where: { i in !(ast[i] is FunDecl) }) {
      return matches
    }

    // Look for members declared inherited by conformance/refinement.
    guard let traits = conformedTraits(of: type, inScope: scope) else { return matches }
    for trait in traits {
      if type == .trait(trait) { continue }

      // TODO: Read source of conformance to disambiguate associated names
      let newMatches = lookup(name, memberOf: .trait(trait), inScope: scope)
      switch type {
      case .associated,
           .genericTypeParam,
           .trait:
        matches.formUnion(newMatches)

      default:
        // Associated size and type declarations are not inherited by conformance.
        matches.formUnion(newMatches.filter({
          $0.kind != .associatedSizeDecl && $0.kind != .associatedTypeDecl
        }))
      }
    }

    return matches
  }

  /// Returns the extending declarations of `type` exposed to `scope`.
  ///
  /// - Note: The declarations referred by the returned IDs conform to `TypeExtendingDecl`.
  private mutating func extendingDecls(
    of type: Type,
    exposedTo scope: AnyNodeID
  ) -> [AnyDeclID] {
    assert(scope.kind <= .lexicalScope)

    var root = scope
    var matches: [AnyDeclID] = []
    let canonicalType = canonicalize(type: type)

    func search(this: inout TypeChecker, inScope scope: AnyNodeID) {
      let decls = this.scopeHierarchy.containees[scope, default: []]
      for i in decls where i.kind == .conformanceDecl || i.kind == .extensionDecl {
        // Skip extending declarations that are being bound.
        guard this.extensionsUnderBinding.insert(i).inserted else { continue }
        defer { this.extensionsUnderBinding.remove(i) }

        // Bind the extension to the extended type.
        guard let subject = this.realize(decl: i) else { continue }

        // Check for a match.
        if this.canonicalize(type: subject) == canonicalType {
          matches.append(i)
        }
      }
    }

    // Look for extension declarations in all visible scopes.
    for scope in scopeHierarchy.scopesToRoot(from: scope) {
      search(this: &self, inScope: scope)
      root = scope
    }

    // Look for extension declarations in imported modules.
    for module in ast.modules where module != root {
      search(this: &self, inScope: AnyNodeID(module))
    }

    return matches
  }

  /// Returns the names and declarations introduced in `scope`.
  private func names<T: NodeIDProtocol>(introducedIn scope: T) -> LookupTable {
    assert(scope.kind <= .lexicalScope)
    guard let decls = scopeHierarchy.containees[scope] else { return [:] }

    var table: LookupTable = [:]
    for i in decls {
      switch i.kind {
      case .associatedSizeDecl,
           .associatedTypeDecl,
           .genericSizeParamDecl,
           .genericTypeParamDecl,
           .namespaceDecl,
           .paramDecl,
           .productTypeDecl,
           .traitDecl,
           .typeAliasDecl:
        let name = (ast[i] as! SingleEntityDecl).name
        table[name, default: []].insert(AnyDeclID(i))

      case .funDecl:
        let decl = ast[NodeID<FunDecl>(converting: i)!]
        guard let name = decl.identifier?.value else { continue }
        switch decl.body?.value {
        case .block, .expr, nil:
          table[name, default: []].insert(AnyDeclID(i))

        case .bundle(let impls):
          modifying(&table[name, default: []], { entries in
            for j in impls {
              entries.insert(AnyDeclID(j))
            }
          })
        }

      case .methodImplDecl:
        let decl = ast[NodeID<FunDecl>(converting: scope)!]
        guard let name = decl.identifier?.value else { continue }
        table[name, default: []].insert(AnyDeclID(i))

      case .subscriptDecl:
        let decl = ast[NodeID<SubscriptDecl>(converting: i)!]
        let name = decl.identifier?.value ?? "[]"
        modifying(&table[name, default: []], { entries in
          for j in decl.impls {
            entries.insert(AnyDeclID(j))
          }
        })

      case .subscriptImplDecl:
        let decl = ast[NodeID<SubscriptDecl>(converting: scope)!]
        let name = decl.identifier?.value ?? "[]"
        table[name, default: []].insert(AnyDeclID(i))

      default:
        unreachable("unexpected declaration")
      }
    }

    // Note: Results should be memoized.
    return table
  }

  // MARK: Type realization

  /// Realizes and returns the type denoted by `expr` evaluated in `scope`.
  private mutating func realize(
    _ expr: AnyTypeExprID,
    inScope scope: AnyNodeID
  ) -> Type? {
    assert(scope.kind <= .lexicalScope)

    switch expr.kind {
    case .conformanceLensTypeExpr:
      return realize(conformanceLens: NodeID(converting: expr)!, inScope: scope)
    case .nameTypeExpr:
      return realize(name: NodeID(converting: expr)!, inScope: scope)
    case .tupleTypeExpr:
      return realize(tuple: NodeID(converting: expr)!, inScope: scope)
    default:
      unreachable("unexpected type expression")
    }
  }

  /// Realizes and returns the type of `Self` in `scope`.
  ///
  /// - Note: This method does not issue diagnostics.
  public mutating func realizeSelfTypeExpr(inScope scope: AnyNodeID) -> Type? {
    assert(scope.kind <= .lexicalScope)

    for scope in scopeHierarchy.scopesToRoot(from: scope) {
      switch scope.kind {
      case .traitDecl:
        let decl = NodeID<TraitDecl>(converting: scope)!
        return .genericTypeParam(GenericTypeParamType(decl: decl, ast: ast))

      case .productTypeDecl,
           .typeAliasDecl:
        fatalError("not implemented")

      default:
        continue
      }
    }

    return nil
  }

  private mutating func realize(
    conformanceLens i: NodeID<ConformanceLensTypeExpr>,
    inScope scope: AnyNodeID
  ) -> Type? {
    let decl = ast[i]
    guard let wrapped = realize(decl.wrapped, inScope: scope) else { return nil }
    guard let trait = realize(decl.focus, inScope: scope) else { return nil }

    /// The focus must be a trait.
    guard case .trait(let focus) = trait else {
      diagnostics.insert(.nonTraitType(trait, range: ast.ranges[decl.focus]))
      return nil
    }

    // The base must conform to the focus.
    guard conformedTraits(of: wrapped, inScope: scope)?.contains(focus) ?? false else {
      diagnostics.insert(.noConformance(of: wrapped, to: focus, range: ast.ranges[decl.focus]))
      return nil
    }

    return .conformanceLens(ConformanceLensType(wrapped: wrapped, focus: focus))
  }

  private mutating func realize(
    name i: NodeID<NameTypeExpr>,
    inScope scope: AnyNodeID
  ) -> Type? {
    assert(scope.kind <= .lexicalScope)

    let identifier = ast[i].identifier
    var base: Type?

    if let j = ast[i].domain {
      // Lookup for the name's identifier in the context of the domain.
      guard let domain = realize(j, inScope: scope) else { return nil }
      let matches = lookup(identifier.value, memberOf: domain, inScope: scope)

      // Realize the referred type.
      for match in matches where match.kind <= .typeDecl {
        if base != nil {
          diagnostics.insert(.ambiguousTypeReference(
            name: identifier.value, range: identifier.range))
          return nil
        }

        if match.kind <= .associatedTypeDecl {
          let decl = NodeID<AssociatedTypeDecl>(converting: match)!
          switch domain {
          case .associated, .conformanceLens, .genericTypeParam:
            base = .associated(AssociatedType(decl: decl, domain: domain, ast: ast))
          default:
            diagnostics.insert(.invalidAssociatedTypeExpr(ast[decl].name, range: identifier.range))
            return nil
          }
        } else {
          base = realize(decl: match)
        }
      }

      if base == nil {
        diagnostics.insert(.noType(named: identifier.value, in: domain, range: identifier.range))
        return nil
      }
    } else {
      // Bypass unqualified lookup for reserved type names.
      switch identifier.value {
      case "Any":   return .any
      case "Never": return .never
      case "Self":
        if let type = realizeSelfTypeExpr(inScope: scope) {
          return type
        } else {
          diagnostics.insert(.invalidSelfTypeExpr(range: identifier.range))
          return nil
        }
      default:
        break
      }

      // Search for the referred type declaration with unqualified lookup.
      let matches = lookup(unqualified: identifier.value, inScope: scope)

      // Realize the referred type.
      for match in matches where match.kind <= .typeDecl {
        if base != nil {
          diagnostics.insert(
            .ambiguousTypeReference(name: identifier.value, range: identifier.range))
          return nil
        }

        if match.kind <= .associatedTypeDecl {
          // Assume `Self` denotes the implicit generic parameter of a trait declaration, since
          // associated declarations cannot be looked up unqualified outside the scope of a trait
          // and its extensions.
          let domain = realizeSelfTypeExpr(inScope: scope)!
          let type = AssociatedType(decl: NodeID(converting: match)!, domain: domain, ast: ast)
          base = .associated(type)
        } else {
          base = realize(decl: match)
        }
      }

      if base == nil {
        diagnostics.insert(.noType(named: identifier.value, range: identifier.range))
        return nil
      }
    }

    // Evaluate the arguments of the referred type, if any.
    if ast[i].arguments.isEmpty {
      return base!
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

      return .boundGeneric(BoundGenericType(base!, arguments: arguments))
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

  /// Realizes and returns the traits of the specified conformance list, or `nil` if at least one
  /// of them is ill-formed.
  private mutating func realize(
    conformances: [NodeID<NameTypeExpr>],
    inScope scope: AnyNodeID
  ) -> Set<TraitType>? {
    // Realize the traits in the conformance list.
    var traits: Set<TraitType> = []
    for expr in conformances {
      guard let rhs = realize(name: expr, inScope: scope) else { return nil }
      if case .trait(let trait) = rhs {
        traits.insert(trait)
      } else {
        diagnostics.insert(.conformanceToNonTraitType(rhs, range: ast.ranges[expr]))
        return nil
      }
    }

    return traits
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

    case .bindingDecl:
      _ = check(binding: NodeID(converting: i)!)
      return declTypes[i]!

    case .conformanceDecl,
         .extensionDecl:
      return _realize(decl: i, { (this, i) in
        let decl = this.ast[i] as! TypeExtendingDecl
        return this.realize(decl.subject, inScope: this.scopeHierarchy.container[i]!)
      })

    case .productTypeDecl:
      return _realize(decl: i, { (this, i) in
        .product(ProductType(decl: NodeID(converting: i)!, ast: this.ast))
      })

    case .subscriptDecl:
      return realize(subscriptDecl: NodeID(converting: i)!)

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

  private mutating func realize(subscriptDecl i: NodeID<SubscriptDecl>) -> Type? {
    _realize(decl: i, { (this, i) in
      let decl = this.ast[i]

      // Realize the input types.
      var inputs: [SubscriptType.Parameter] = []
      if decl.parameters != nil { fatalError("not implemented") }

      if let parent = this.scopeHierarchy.container[i] {
        switch parent.kind {
        case .productTypeDecl,
             .traitDecl,
             .typeAliasDecl:
          let receiver = this.realizeSelfTypeExpr(inScope: parent)!
          inputs.insert(SubscriptType.Parameter(label: nil, type: receiver), at: 0)

        default:
          break
        }
      }

      // Realize the ouput type an collect capabilities.
      guard let output = this.realize(decl.output, inScope: AnyNodeID(i)) else { return nil }
      let capabilities = Set(decl.impls.map({ this.ast[$0].introducer.value }))

      return .subscript(SubscriptType(
        isProperty: decl.parameters == nil,
        capabilities: capabilities,
        inputs: inputs,
        output: output))
    })
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
      diagnostics.insert(.circularDependency(range: ast.ranges[i]))
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
