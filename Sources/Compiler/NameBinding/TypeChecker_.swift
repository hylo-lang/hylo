import Utils

/// Val's type checker.
struct TypeChecker_ {

  /// The status of a type checking request on a declaration.
  private enum RequestStatus {

    /// Type realization has started.
    ///
    /// The checker is realizing the type of the declaration. Initiating a new type realization or
    /// type checking request on the declaration will cause a circular dependency error.
    case typeRealizationStarted

    /// Type realization was completed.
    ///
    /// The checker realized the type of the declaration, which is available in `declTypes`.
    case typeRealizationCompleted

    /// Type checking has started.
    ///
    /// The checker is verifying whether the declaration is well-formed; its type is available in
    /// `declTypes`. Initiating a new type checking request will cause a circular dependency error.
    case typeCheckingStarted

    /// Type checking succeeded.
    ///
    /// The declaration is well-formed; its type is availabe in `declTypes`.
    case success

    /// Type checking failed.
    case failure

  }

  /// The diagnostics of the type errors.
  var diags: [Diag] = []

  /// The name binder.
  var binder: NameBinder

  /// The type of each declared entity.
  fileprivate(set) var declTypes: [Decl.ID: ValType] = [:]

  /// The type of each expression, inferred during type checking.
  ///
  /// Type information is first approximated from the root of an expression to its leaves, using
  /// type annotations. Then, it is refined by building type constraints based on the structure of
  /// the AST, from leaves to root.
  fileprivate(set) var exprTypes: [Expr.ID: ValType] = [:]

  /// A cache for type checking requests on declarations.
  private var declRequests: [Decl.ID: RequestStatus] = [:]

  /// A cache for type checking requests on type signatures.
  private var signRequests: [Sign.ID: ValType] = [:]

  /// A cache for the uninstantiated type constraints extracted from generic clauses.
  private var clauseRequirements: [Decl.ID: [RelationalConstraint]] = [:]

  /// Creates a new type checker.
  ///
  /// - Parameters:
  ///   - modules: The modules that are visible during name binding.
  ///   - stdlib: The module representing the standard library, if it is loaded.
  init(modules: [String: ModuleDecl], stdlib: ModuleDecl?) {
    self.binder = NameBinder(modules: modules, stdlib: stdlib)
  }

  // MARK: Declarations

  /// Type checks `decl` and returns whether that succeeded.
  ///
  /// - Note: This method is idempotent.
  mutating func check(decl: Decl) -> Bool {
    // Check if we already processed the request.
    loop:while true {
      switch declRequests[decl.id] {
      case nil:
        // Realize the type of the declaration before starting type checking.
        _  = realize(typeOf: decl)
        assert(declRequests[decl.id] != nil)

      case .typeRealizationCompleted:
        declRequests[decl.id] = .typeCheckingStarted
        break loop

      case .typeRealizationStarted, .typeCheckingStarted:
        fatalError("circular dependency")

      case .success:
        return true

      case .failure:
        return false
      }
    }

    // Dispatch.
    let success: Bool
    switch decl {
    case let decl as ModuleDecl:
      success = check(iterable: decl)
    case let decl as NamespaceDecl:
      success = check(iterable: decl)
    case let decl as ExtensionDecl:
      success = check(iterable: decl)
    case let decl as ProductTypeDecl:
      success = check(nominalType: decl)
    case let decl as ViewTypeDecl:
      success = check(nominalType: decl)
    case let decl as AliasTypeDecl:
      success = check(aliasType: decl)
    case let decl as AbstractTypeDecl:
      success = check(abstractType: decl)
    case let decl as PatternBindingDecl:
      success = check(binding: decl)
    case let decl as VarDecl:
      success = check(vardecl: decl)
    case let decl as BaseFunDecl:
      success = check(fun: decl)
    default:
      fatalError("unexpected declaration \(type(of: decl))")
    }

    // Update the type checking request status and return.
    declRequests[decl.id] = success ? .success : .failure
    return success
  }

  private mutating func check<D>(iterable decl: D) -> Bool where D: IterableDeclSpace {
    decl.decls.reduce(into: true, { (result, member) in
      result = check(decl: member) && result
    })
  }

  private mutating func check(nominalType decl: NominalTypeDecl) -> Bool {
    // Make sure the generic clause is well-formed.
    var success = checkClause(of: decl)

    // Type check the type's direct members.
    for member in decl.directMembers {
      success = check(decl: member) && success
    }

    // Type check extensions.
    for ext in binder.extensions(of: decl) {
      success = check(decl: ext) && success
    }

    // TODO: Check the type's conformance

    return success
  }

  private mutating func check(aliasType decl: AliasTypeDecl) -> Bool {
    return !declTypes[decl.id]![.hasErrors]
  }

  private mutating func check(abstractType decl: AbstractTypeDecl) -> Bool {
    return !declTypes[decl.id]![.hasErrors]
  }

  /// - Note: This method might be called from type realization.
  private mutating func check(binding decl: PatternBindingDecl) -> Bool {
    var system = ConstraintSystem()
    let useSite = decl.parentDeclSpace!

    var type: ValType
    if let sign = decl.sign {
      // There is a type annotation; use it as the authoritative type information.
      type = realize(sign, useSite: useSite)

      // Contextualize the type signature.
      type = contextualize(type: type, from: useSite, system: &system)

      if var expr = decl.initializer {
        // There's an initializer; type check it.
        let (success, solution) = infer(
          expr: &expr, fixedType: type, from: useSite, system: &system)
        decl.initializer = expr

        if success {
          type = solution.reify(type, substPolicy: .bindToError)
        } else {
          decl.pattern.type = .error
          return false
        }
      } else if type[.hasVariables] {
        // The annotation has open type parameters that couldn't be infered.
        diags.append(.typeRequiresArguments(type, range: sign.range))
        decl.pattern.type = .error
        return false
      }
    } else if var expr = decl.initializer {
      // There's no type annotation but there's an initializer.
      let (success, solution) = infer(expr: &expr, from: useSite, system: &system)
      decl.initializer = expr

      if success {
        type = solution.reify(exprTypes[expr.id]!, substPolicy: .bindToError)
      } else {
        decl.pattern.type = .error
        return false
      }
    } else {
      // No explicit signature and no initializer.
      diags.append(.missingTypeAnnotation(range: decl.pattern.range))
      decl.pattern.type = .error
      return false
    }

    // At this point we shouldn't have any type variable left.
    assert(!type[.hasVariables])
    declTypes[decl.id] = type.uncontextualized

    // Make sure that the pattern is compatible with the inferred type.
    guard assign(type: type, to: decl.pattern) else {
      decl.pattern.type = .error
      for pattern in decl.pattern.namedPatterns {
        declTypes[pattern.decl.id] = .error
        declRequests[pattern.decl.id] = .failure
      }
      return false
    }

    return true
  }

  private mutating func check(vardecl decl: VarDecl) -> Bool {
    // If the variable is not associated with a pattern binding declaration, it must be introduced
    // by a match or a conditional expression.
    if let binding = decl.patternBindingDecl {
      return check(decl: binding)
    } else {
      // FIXME: Match cases should be pattern binding declarations.
      fatalError("unreachable")
    }
  }

  private mutating func check(fun decl: BaseFunDecl) -> Bool {
    assert(!decl.isSynthesized, "synthesized functions cannot be type checked")

    guard
      let type = declTypes[decl.id] as? FunType,
      !type[.hasErrors]
    else { return false }

    // Make sure the generic clause is well-formed.
    var success = checkClause(of: decl)

    // Type check the function's captures.
    for capture in decl.explicitCaptures {
      success = check(decl: capture) && success
    }

    // Type check the function's body, if any.
    if var expr = decl.singleExprBody {
      var system = ConstraintSystem()
      let body = decl.body!
      let returnType = contextualize(type: type.retType, from: body, system: &system)

      // The body's type must be some subtype of `returnType`.
      let bodyType = TypeVar(node: body)
      system.insert(RelationalConstraint(
        kind: .subtyping, lhs: bodyType, rhs: returnType,
        at: ConstraintLocator(decl, .returnType)))

      let (success_, _) = infer(expr: &expr, fixedType: bodyType, from: body, system: &system)
      success = success_ && success

      decl.body = BraceStmt(statements: [expr], range: expr.range)
    } else if let body = decl.body {
      success = check(stmt: body, from: decl) && success
    }

    return success
  }

  /// Realizes and returns the type of the entity defined by `decl`.
  ///
  /// - Note: This method is idempotent.
  mutating func realize(typeOf decl: Decl) -> ValType {
    // Check if we already processed the request.
    switch declRequests[decl.id] {
    case nil:
      declRequests[decl.id] = .typeRealizationStarted

    case .typeRealizationStarted:
      fatalError("circular dependency")

    case .typeRealizationCompleted, .typeCheckingStarted, .success, .failure:
      return declTypes[decl.id]!
    }

    // Dispatch.
    switch decl {
    case let decl as ModuleDecl:
      declTypes[decl.id] = ModuleType(decl: decl)
    case let decl as NamespaceDecl:
      declTypes[decl.id] = NamespaceType(decl: decl)
    case let decl as ProductTypeDecl:
      declTypes[decl.id] = ProductType(decl: decl)
    case let decl as ViewTypeDecl:
      declTypes[decl.id] = ViewType(decl: decl)
    case let decl as AbstractTypeDecl:
      declTypes[decl.id] = GenericParamType(decl: decl)
    case let decl as AliasTypeDecl:
      declTypes[decl.id] = realize(typeOfAliasType: decl)
    case let decl as BaseFunDecl:
      declTypes[decl.id] = realize(typeOfFun: decl)

    case let decl as PatternBindingDecl:
      // Type realization of pattern binding declarations requires type checking.
      declRequests[decl.id] = .typeCheckingStarted
      declRequests[decl.id] = check(binding: decl)
        ? .success
        : .failure
      return declTypes[decl.id]!

    case let decl as VarDecl:
      // Type realization of variable declarations requires type checking of the surrounding
      // pattern binding declaration.
      _ = check(binding: decl.patternBindingDecl!)
      return declTypes[decl.id]!

    default:
      fatalError("unexpected declaration \(Swift.type(of: decl))")
    }

    // Update the type checking request status and return.
    declRequests[decl.id] = .typeRealizationCompleted
    return declTypes[decl.id]!
  }

  private mutating func realize(typeOfAliasType decl: AliasTypeDecl) -> ValType {
    return realize(decl.aliasedSign, useSite: decl).kind
  }

  private mutating func realize(typeOfFun decl: BaseFunDecl) -> ValType {
    if (decl is CtorDecl) && decl.isSynthesized {
      // Realize the type of all stored properties.
      let typeDecl = decl.parentDeclSpace as! NominalTypeDecl
      let params = typeDecl.storedVars.map({ (vardecl) -> FunType.Param in
        FunType.Param(label: vardecl.ident, policy: .consuming, rawType: realize(typeOf: vardecl))
      })

      return FunType(params: params, retType: decl.selfDecl!.type)
    }

    // Realize the applied type of the function.
    let params = decl.params.map({ (param) -> FunType.Param in
      let sign = param.sign ?? fatalError("cannot realize implicit parameter type")
      assert((sign as! FunParamSign).policy == param.policy)
      return FunType.Param(label: param.label, type: realize(sign, useSite: decl))
    })

    let returnType: ValType
    if decl is CtorDecl {
      returnType = decl.selfDecl!.type
    } else if let sign = decl.retSign {
      returnType = realize(sign, useSite: decl)
    } else {
      returnType = .unit
    }

    return FunType(params: params, retType: returnType)
  }

  // MARK: Statements

  /// Type checks `stmt` and returns whether that succeeded.
  ///
  /// - Note: This method is **not** idempotent.
  mutating func check(stmt: Stmt, from useSite: DeclSpace) -> Bool {
    switch stmt {
    case let stmt as BraceStmt:
      return check(brace: stmt, from: useSite)
    case let stmt as RetStmt:
      return check(ret: stmt, from: useSite)
    default:
      fatalError("unexpected statement \(type(of: stmt))")
    }
  }

  private mutating func check(brace stmt: BraceStmt, from useSite: DeclSpace) -> Bool {
    var success = true
    for i in 0 ..< stmt.stmts.count {
      switch stmt.stmts[i] {
      case let node as Decl:
        success = check(decl: node) && success

      case let node as Stmt:
        success = check(stmt: node, from: stmt)

      case var node as Expr:
        var system = ConstraintSystem()
        success = infer(expr: &node, from: useSite, system: &system).success && success
        stmt.stmts[i] = node

      case let node:
        fatalError("unexpected AST node \(type(of: node))")
      }
    }

    return success
  }

  private mutating func check(ret stmt: RetStmt, from useSite: DeclSpace) -> Bool {
    let fun = useSite.spacesUpToRoot.first(as: BaseFunDecl.self)
      ?? fatalError("return statement outside of a function")

    // Bail out if we can't realize the type of the function.
    guard let funType = realize(typeOf: fun) as? FunType else { return false }

    // If the statement doesn't have a return value, the function must return `()`.
    guard var value = stmt.value else {
      if funType.retType == .unit {
        return true
      } else {
        diags.append(.missingReturnValue(range: stmt.range))
        return false
      }
    }

    // Constrain the type of the return value.
    var system = ConstraintSystem()
    let retType = contextualize(type: funType.retType, from: useSite, system: &system)
    let fixedType = TypeVar()
    system.insert(RelationalConstraint(
      kind: .subtyping, lhs: fixedType, rhs: retType, at: ConstraintLocator(stmt, .returnValue)))

    let (success, _) = infer(expr: &value, fixedType: fixedType, from: useSite, system: &system)
    stmt.value = value
    return success
  }

  // MARK: Expressions

  /// Infers the type of `expr` and returns whether it's well-formed, along with the best solution
  /// from the type inferrer.
  ///
  /// - Note: This method is **not** idempotent.
  mutating func infer(
    expr: inout Expr,
    fixedType: ValType? = nil,
    from useSite: DeclSpace,
    system: inout ConstraintSystem,
    substPolicy: FreeTypeVarSubstPolicy = .bindToError
  ) -> (success: Bool, solution: Solution) {
    // Associate the expression with the fixed type.
    exprTypes[expr.id] = fixedType ?? TypeVar(node: expr)

    // Temporarily projects `self` into the constraint generator.
    expr = withBorrowedSelf({ (this) -> (Self, Expr) in
      var generator = CSGenerator(checker: this, useSite: useSite, system: system)
      expr = expr.accept(&generator)
      system = generator.system.release()
      return (generator.checker.release(), expr)
    })

    // Solve the constraint system.
    var solver = CSSolver(system: system, context: _ctx)
    let solution = solver.solve()
    expr.forEach({ subexpr in
      exprTypes[subexpr.id] = solution.reify(
        exprTypes[subexpr.id, default: .error], substPolicy: substPolicy)
      return true
    })

    return (success: solution.errors.isEmpty, solution: solution)
  }

  // MARK: Type signatures

  /// Realizes and returns the type denoted by `sign`.
  ///
  /// - Parameter useSite: the innermost declaration space in which `sign` occurs.
  /// - Note: This method is idempotent.
  fileprivate mutating func realize(_ sign: Sign, useSite: DeclSpace) -> ValType {
    if let type = signRequests[sign.id] {
      return type
    }

    // Dispatch.
    switch sign {
    case let sign as TupleSign:
      signRequests[sign.id] = realize(tuple: sign, useSite: useSite)
    case let sign as FunSign:
      signRequests[sign.id] = realize(fun: sign, useSite: useSite)
    case let sign as FunParamSign:
      signRequests[sign.id] = realize(param: sign, useSite: useSite)
    case let sign as AsyncSign:
      signRequests[sign.id] = realize(async: sign, useSite: useSite)
    case let sign as UnionSign:
      signRequests[sign.id] = realize(union: sign, useSite: useSite)
    case let sign as ViewCompSign:
      signRequests[sign.id] = realize(viewComp: sign, useSite: useSite)
    case let sign as BareNameSign:
      signRequests[sign.id] = realize(bareName: sign, useSite: useSite)
    case let sign as SpecializedNameSign:
      signRequests[sign.id] = realize(specializedName: sign, useSite: useSite)
    case let sign as MemberSign:
      signRequests[sign.id] = realize(member: sign, useSite: useSite)
    default:
      fatalError("unexpected signature \(type(of: sign))")
    }

    return signRequests[sign.id]!
  }

  private mutating func realize(tuple sign: TupleSign, useSite: DeclSpace) -> ValType {
    var elems: [TupleType.Elem] = []
    for elem in sign.elems {
      if (elem.label == nil) || !elems.contains(where: { $0.label == elem.label }) {
        elems.append(TupleType.Elem(
          label: elem.label, type: realize(elem.sign, useSite: useSite)))
      } else {
        diags.append(.duplicateTupleLabel(range: elem.range))
      }
    }
    return TupleType(elems)
  }

  private mutating func realize(fun sign: FunSign, useSite: DeclSpace) -> ValType {
    let params = sign.params.map({ param in
      FunType.Param(label: param.label, type: realize(param, useSite: useSite))
    })
    let retType = realize(sign.retSign, useSite: useSite)
    return FunType(params: params, retType: retType)
  }

  private mutating func realize(param sign: FunParamSign, useSite: DeclSpace) -> ValType {
    FunParamType(policy: sign.policy, rawType: realize(sign.rawSign, useSite: useSite))
  }

  private mutating func realize(async sign: AsyncSign, useSite: DeclSpace) -> ValType {
    AsyncType(base: realize(sign.base, useSite: useSite))
  }

  private mutating func realize(union sign: UnionSign, useSite: DeclSpace) -> ValType {
    UnionType(sign.elems.map({ realize($0, useSite: useSite) }))
  }

  private mutating func realize(viewComp sign: ViewCompSign, useSite: DeclSpace) -> ValType {
    var failed = false
    var elems: [ViewType] = []

    for elem in sign.views {
      switch realize(elem, useSite: useSite) {
      case let type as ViewType:
        elems.append(type)

      case is ErrorType:
        // The error has already been diagnosed.
        failed = true

      case let type:
        // We didn't realize a view type.
        diags.append(.nonViewTypeConformanceRequirement(type: type, range: elem.range))
        failed = true
      }
    }

    return failed ? .error : ViewCompositionType(elems)
  }

  private mutating func realize(bareName sign: BareNameSign, useSite: DeclSpace) -> ValType {
    realize(unqualified: sign, useSite: useSite)
  }

  private mutating func realize(
    specializedName sign: SpecializedNameSign,
    useSite: DeclSpace
  ) -> ValType {
    let type = realize(sign.bare, useSite: useSite)

    guard let type = type as? NominalType,
          let clause = type.decl.genericClause
    else {
      diags.append(.cannotSpecializeNonGenericType(type, range: sign.range))
      return .error
    }

    guard clause.params.count >= sign.args.count else {
      diags.append(.tooManyTypeArguments(
        expected: clause.params.count, got: sign.args.count, range: sign.range))
      return .error
    }

    // Realize the type arguments.
    var args: [ValType] = []
    var bindings: [GenericParamType: ValType] = [:]
    for i in 0 ..< clause.params.count {
      let arg = realize(sign.args[i], useSite: useSite)
      args.append(arg)
      bindings[GenericParamType(decl: clause.params[i])] = arg
    }

    // Check that type arguments satisfy type requirements.
    var system = ConstraintSystem()
    var constraints = requirements(of: type.decl)
    for i in 0 ..< constraints.count {
      constraints[i].lhs = constraints[i].lhs.specialized(with: bindings)
      constraints[i].rhs = constraints[i].rhs.specialized(with: bindings)
      constraints[i].locator = ConstraintLocator(sign)
      system.insert(constraints[i])
    }

    var solver = CSSolver(system: system, context: _ctx)
    let solution = solver.solve()
    solution.reportErrors(into: &diags)

    return BoundGenericType(decl: type.decl, args: args)
  }

  private mutating func realize(member sign: MemberSign, useSite: DeclSpace) -> ValType {
    let base = realize(sign.base, useSite: useSite)

    guard let base = base as? NominalType,
          let decl = binder.lookup(sign.ident, qualifiedBy: base.decl).type
    else {
      diags.append(.noType(named: sign.ident, in: base, range: sign.identRange))
      return .error
    }

    return decl.instanceType
  }

  /// Realizes a single, unqualified component name.
  private mutating func realize(unqualified sign: NameSign, useSite: DeclSpace) -> ValType {
    guard let decl = binder.resolve(sign, unqualifiedFrom: useSite) else {
      // The error has been diagnosed by the name binder.
      return .error
    }

    // If the signature refers to an abstract type. We must create an associated type encoding its
    // relationship to the defining view. In other words, if `A` is abstract, we realize `Self.A`
    // rather than just `A`.
    if let decl = decl as? AbstractTypeDecl {
      let viewDecl = decl.parentDeclSpace as! ViewTypeDecl
      let viewSelf = viewDecl.selfTypeDecl.instanceType
      return AssocType(interface: decl.instanceType as! GenericParamType, base: viewSelf)
    }

    return decl.instanceType
  }

  /// Returns whether `decl`'s generic clause is well-formed.
  private mutating func checkClause(of decl: BaseGenericDecl) -> Bool {
    if let clause = decl.genericClause {
      return requirements(of: decl).count == clause.typeReqs.count
    } else {
      return true
    }
  }

  /// Returns the well-formed type requirements of `decl`'s generic clause in the form of type
  /// (unspecialized) constraints.
  ///
  /// - Note: This method is idempotent.
  private mutating func requirements(of decl: BaseGenericDecl) -> [RelationalConstraint] {
    if let reqs = clauseRequirements[decl.id] {
      return reqs
    }

    guard let clause = decl.genericClause else {
      clauseRequirements[decl.id] = []
      return []
    }

    func isValidLHS(_ lhs: ValType) -> Bool {
      switch lhs {
      case let lhs as GenericParamType:
        return clause.params.contains(where: { $0.id == lhs.decl.id })
      case let lhs as AssocType:
        return isValidLHS(lhs.root)
      default:
        return false
      }
    }

    let locator = ConstraintLocator(clause)
    var results: [RelationalConstraint] = []
    for req in clause.typeReqs {
      // Realize the two operands.
      let lhs = realize(req.lhs, useSite: decl)
      let rhs = realize(req.rhs, useSite: decl)

      // Bail out if there's an error.
      guard !lhs[.hasErrors] && !rhs[.hasErrors] else { continue }

      // LHS must be a parameter or an associated type introduced by the type declaration.
      guard isValidLHS(lhs) else {
        diags.append(.notInEnvironment(lhs, range: req.lhs.range))
        continue
      }

      switch req.kind {
      case .equality:
        results.append(RelationalConstraint(kind: .equality, lhs: lhs, rhs: rhs, at: locator))

      case .conformance:
        // RHS must be a view or view composition in conformance requirements.
        guard rhs is ViewType || rhs is ViewCompositionType else {
          diags.append(.nonViewTypeRequirement(on: rhs, range: req.rhs.range))
          continue
        }
        results.append(RelationalConstraint(kind: .conformance, lhs: lhs, rhs: rhs, at: locator))
      }
    }

    clauseRequirements[decl.id] = results
    return results
  }

  // MARK: Helpers

  /// Contextualizes `type` in the context of `useSite`.
  mutating func contextualize(
    type: ValType,
    from useSite: DeclSpace,
    system: inout ConstraintSystem
  ) -> ValType {
    // Nothing to do if the type isn't parameterized.
    guard type[.hasTypeParams] else { return type }

    // Substitute type parameters with skolems and fresh variables.
    var walker = Contextualizer_(from: useSite, binder: binder)
    let contextualized = walker.walk(type)
    binder = walker.binder

    // TODO: Contextualize type requirments from the innermost generic environment.

    // TODO: Synthesize missing type arguments.

    // Check if we have to synthetize additional generic arguments, in case the signature refers
    // to an "underspecialized" generic nominal type.
    return contextualized
  }

  private mutating func assign(type: ValType, to pattern: Pattern) -> Bool {
    switch pattern {
    case let pattern as NamedPattern:
      pattern.type = type
      declTypes[pattern.decl.id] = type.uncontextualized
      declRequests[pattern.decl.id] = .success
      return true

    case let pattern as TuplePattern:
      if let type = type as? TupleType {
        if pattern.elems.count == type.elems.count {
          // The pattern and the type are tuples of the same length.
          pattern.type = type
          var success = true
          for (t, p) in zip(type.elems, pattern.elems) {
            if (p.label == nil) || (p.label == t.label) {
              success = assign(type: t.type, to: p.pattern) && success
            } else {
              diags.append(.incompatibleTupleLabel(range: p.range))
              success = false
            }
          }
          return success
        } else if (pattern.elems.count == 1) && (pattern.elems[0].label == nil) {
          // Allow `(pattern): (A, B, C)`.
          pattern.type = type
          return assign(type: type, to: pattern.elems[0].pattern)
        } else {
          // Incompatible lengths.
          diags.append(.incompatibleTupleLengths(range: pattern.range))
          return false
        }
      } else if (pattern.elems.count == 1) && (pattern.elems[0].label == nil) {
        // Allow `(pattern): A`.
        pattern.type = type
        return assign(type: type, to: pattern.elems[0].pattern)
      } else {
        // Illegal destructuring.
        diags.append(.cannotDestructure(type, range: pattern.range))
        return false
      }

    case is WildcardPattern:
      pattern.type = type
      return true

    default:
      fatalError("unreachable")
    }
  }

  private mutating func withBorrowedSelf<T>(_ action: (Self) -> (Self, T)) -> T {
    withUnsafeMutablePointer(to: &self, { this in
      let (s, result) = action(this.move())
      this.initialize(to: s)
      return result
    })
  }

}

struct Contextualizer_: TypeWalker {

  var parent: ValType?

  /// The space from wich the visited type is being used.
  ///
  /// Generic parameters that are used from a an enclosed use site are skolemized. Others are
  /// opened as fresh type variables.
  var useSite: DeclSpace

  /// A name binder.
  var binder: NameBinder

  /// The substitution table keeping track of the type variables that were used to open each
  /// specific generic type parameter.
  var substitutions: [GenericParamType: TypeVar] = [:]

  init(from useSite: DeclSpace, binder: NameBinder) {
    self.useSite = useSite
    self.binder = binder
  }

  mutating func willVisit(_ type: ValType) -> TypeWalkerAction {
    guard let type = type as? GenericParamType else {
      return type[.hasTypeParams] ? .stepInto(type) : .stepOver(type)
    }

    if let variable = substitutions[type] {
      // We already opened the parameter.
      return .stepOver(variable)
    } else if shouldSkolemize(type) {
      // Skolemize the parameter.
      return .stepOver(SkolemType(interface: type))
    } else {
      // Open the parameter as a fresh variable.
      let variable = TypeVar()
      substitutions[type] = variable
      return .stepOver(variable)
    }
  }

  /// Returns whether `type` should be skolemized.
  private func shouldSkolemize(_ type: GenericParamType) -> Bool {
    // Identify the declaration space that introduces the parameter.
    let origin = type.decl.parentDeclSpace?.spacesUpToRoot.first(where: { space in
      if let space = space as? BaseGenericDecl,
         let clause = space.genericClause
      {
        return clause.params.contains(where: { $0.id == type.decl.id })
      } else {
        return false
      }
    }) ?? fatalError("unbound type parameter")

    if useSite is NominalTypeDecl {
      // Members of a nominal type reside directly in its declaration space.
      return (useSite === origin) || useSite.isDescendant(of: origin)
    } else {
      // The body of a function is nested within the function's declaration space. Thus, references
      // from the function's own declaration space (e.g., in default arguments) are external.
      return useSite.isDescendant(of: origin)
    }
  }

}

fileprivate struct CSGenerator: ExprVisitor {

  typealias Result = Expr

  /// A borrowed projection of the type checker that uses this constraint generator.
  var checker: TypeChecker_!

  /// The innermost declaration space in which the expression occurs.
  var useSite: DeclSpace

  /// The constraint system being built.
  var system: ConstraintSystem!

  mutating func visit(_ node: BoolLiteralExpr) -> Expr { node }

  mutating func visit(_ node: IntLiteralExpr) -> Expr { node }

  mutating func visit(_ node: FloatLiteralExpr) -> Expr { node }

  mutating func visit(_ node: StringLiteralExpr) -> Expr { node }

  mutating func visit(_ node: AssignExpr) -> Expr { node }

  mutating func visit(_ node: BaseCastExpr) -> Expr { node }

  mutating func visit(_ node: StaticCastExpr) -> Expr { node }

  mutating func visit(_ node: RuntimeCastExpr) -> Expr { node }

  mutating func visit(_ node: PointerCastExpr) -> Expr { node }

  mutating func visit(_ node: TupleExpr) -> Expr {
    if let type = checker.exprTypes[node.id] as? TupleType,
       type.labels == node.labels
    {
      // Propagate the expected type down the expression tree.
      for i in 0 ..< node.elems.count {
        checker.exprTypes[node.elems[i].value.id] = type.elems[i].type
        node.elems[i].value = node.elems[i].value.accept(&self)
      }
    } else {
      // Infer the tuple's type from the leaves.
      var typeElems: [TupleType.Elem] = []
      for i in 0 ..< node.elems.count {
        node.elems[i].value = node.elems[i].value.accept(&self)
        typeElems.append(TupleType.Elem(
          label: node.elems[i].label, type: checker.exprTypes[node.elems[i].value.id]!))
      }

      let inferredType = TupleType(typeElems)
      if let fixedType = checker.exprTypes[node.id] {
        system.insert(RelationalConstraint(
          kind: .equality, lhs: inferredType, rhs: fixedType, at: ConstraintLocator(node)))
      } else {
        checker.exprTypes[node.id] = inferredType
      }
    }

    assert(checker.exprTypes[node.id] != nil)
    return node
  }

  mutating func visit(_ node: CallExpr) -> Expr {
    // Infer the type of the callee bottom-up.
    node.callee = node.callee.accept(&self)

    if let callee = node.callee as? TypeDeclRefExpr {
      // Rewrite as an explicit constructor call.
      let matches = checker.binder.lookup("new", qualifiedBy: callee.decl)
        .filter({ $0 is CtorDecl })

      if matches.values.isEmpty {
        checker.diags.append(.noBinding(
          named: LabeledIdent(base: "new"), in: callee.decl, range: callee.range))
        checker.exprTypes[node.id] = .error
        return node
      }

      node.callee = bind(declRef: callee, to: matches)
    }

    // There are two cases to consider, depending on whether we could infer a function type for
    // the callee's expression. If we could, we can propagate that information top-down to refine
    // the inference of the arguments' types. If we couldn't, the callee is probably overloaded
    // and we must rely on bottom-up inference to constrain to select the appropriate candidate.

    if let type = checker.exprTypes[node.callee.id] as? FunType,
       type.labels == node.labels
    {
      // Use the callee's return type to infer the type of the call.
      if let fixedType = checker.exprTypes[node.id] {
        system.insert(RelationalConstraint(
          kind: .equality, lhs: type.retType, rhs: fixedType, at: ConstraintLocator(node)))
      } else {
        checker.exprTypes[node.id] = type.retType
      }

      // Propagate the callee's inferred type down to infer the types of the arguments.
      for i in 0 ..< node.args.count {
        // If the parameter `inout`, the argument must have the same type.
        let argType = TypeVar(node: node.args[i].value)
        let kind: RelationalConstraint.Kind = type.params[i].policy == .inout
          ? .equality
          : .paramSubtyping
        system.insert(RelationalConstraint(
          kind: .paramSubtyping, lhs: argType, rhs: type.params[i].type,
          at: ConstraintLocator(node, .argument(i))))

        checker.exprTypes[node.args[i].value.id] = argType
        node.args[i].value = node.args[i].value.accept(&self)
      }
    } else {
      // Infer the callee's return type from the type of the call.
      let returnType = checker.exprTypes[node.id] ?? TypeVar(node: node)

      // Infer the types of the callee's parameters from the arguments.
      var params: [FunType.Param] = []
      for i in 0 ..< node.args.count {
        node.args[i].value = node.args[i].value.accept(&self)

        // If the argument is passed `inout`, the parameter's raw type must be the same as the
        // argument's. Otherwise, the solver must infer the parameter's raw type and passing
        // convention based on the argument's type, using a subtyping constraint.
        let paramType = TypeVar(node: node.args[i].value)
        let kind: RelationalConstraint.Kind = node.args[i] is AddrOfExpr
          ? .equality
          : .paramSubtyping
        system.insert(RelationalConstraint(
          kind: kind, lhs: checker.exprTypes[node.args[i].value.id]!, rhs: paramType,
          at: ConstraintLocator(node, .argument(i))))
        params.append(FunType.Param(label: node.args[i].label, type: paramType))
      }

      let calleeType = FunType(params: params, retType: returnType)
      system.insert(RelationalConstraint(
        kind: .equality, lhs: checker.exprTypes[node.callee.id]!, rhs: calleeType,
        at: ConstraintLocator(node, .application)))
    }

    assert(checker.exprTypes[node.id] != nil)
    return node
  }

  mutating func visit(_ node: UnresolvedDeclRefExpr) -> Expr {
    var matches: LookupResult_
    var useBeforeDecl = false

    // If we're in a brace statement, look for local bindings first.
    if useSite is BraceStmt {
      matches = checker.binder.lookup(node.ident.base, qualifiedBy: useSite)
      if let decl = matches.first, !decl.isOverloadable {
        // If we found a local type declaration, we're done.
        if decl is TypeDecl {
          return bind(declRef: node, to: matches)
        }

        // If we found a local, non-overloadable value declaration, make sure it appears lexically
        // before the `node`.
        if let declLoc = decl.range?.upperBound,
           let nodeLoc = node.range?.lowerBound,
           nodeLoc >= declLoc
        {
          return bind(declRef: node, to: matches)
        }

        // Expand the search for better candidates.
        useBeforeDecl = true
        matches = checker.binder.lookup(
          node.ident.base, unqualifiedFrom: useSite.parentDeclSpace!)
      } else {
        // We found nothing or overloadable declarations in the local scope; expand the search.
        matches = checker.binder.lookup(
          node.ident.base, unqualifiedFrom: useSite.parentDeclSpace!, initialMatches: matches)
      }
    } else {
      matches = checker.binder.lookup(node.ident.base, unqualifiedFrom: useSite)
    }

    if matches.isEmpty {
      if useBeforeDecl {
        checker.diags.append(.bindingUsedBeforeDeclaration(node.ident, range: node.range))
      } else {
        checker.diags.append(.noBinding(named: node.ident, range: node.range))
      }
      checker.exprTypes[node.id] = .error
      return node
    }

    return bind(declRef: node, to: matches)
  }

  mutating func visit(_ node: UnresolvedMemberExpr) -> Expr { node }

  mutating func visit(_ node: UnresolvedQualDeclRefExpr) -> Expr { node }

  mutating func visit(_ node: OverloadedDeclRefExpr) -> Expr { node }

  mutating func visit(_ node: DeclRefExpr) -> Expr { node }

  mutating func visit(_ node: TypeDeclRefExpr) -> Expr { node }

  mutating func visit(_ node: KindRefExpr) -> Expr { node }

  mutating func visit(_ node: MemberDeclRefExpr) -> Expr { node }

  mutating func visit(_ node: TupleMemberExpr) -> Expr { node }

  mutating func visit(_ node: SpecializedDeclRefExpr) -> Expr { node }

  mutating func visit(_ node: LambdaExpr) -> Expr { node }

  mutating func visit(_ node: AsyncExpr) -> Expr { node }

  mutating func visit(_ node: AwaitExpr) -> Expr { node }

  mutating func visit(_ node: AddrOfExpr) -> Expr { node }

  mutating func visit(_ node: MatchExpr) -> Expr { node }

  mutating func visit(_ node: WildcardExpr) -> Expr { node }

  mutating func visit(_ node: ErrorExpr) -> Expr { node }

  /// Desugars an unresolved binding given the set of declarations to which it may refer.
  private mutating func bind(declRef expr: Expr, to matches: LookupResult_) -> Expr {
    assert(!matches.isEmpty)
    var desugared: Expr

    if let decl = matches.type {
      // Rewrite as an explicit type declaration.
      desugared = TypeDeclRefExpr(decl: decl, range: expr.range)
      checker.exprTypes[desugared.id] = checker.realize(typeOf: decl)
      return desugared
    }

    if matches.count > 1 {
      // Rewrite as an overloaded declaration.
      desugared = OverloadedDeclRefExpr(declSet: matches.values, range: expr.range)
      checker.exprTypes[desugared.id] = TypeVar(node: expr)
      return desugared
    }

    let decl = matches.values[0]
    if let expr = expr as? MemberExpr {
      // Rewrite as an explicit value member declaration.
      desugared = MemberDeclRefExpr(base: expr.base, decl: decl, range: expr.range)
    } else if decl.isMember {
      // Rewrite as an implicit value member declaration.
      let receiverDecl = checker.binder.lookup("self", unqualifiedFrom: useSite).values[0]
      let receiverType = checker.realize(typeOf: receiverDecl)
      let receiverExpr = DeclRefExpr(decl: receiverDecl, type: .unresolved, range: expr.range)
      checker.exprTypes[receiverExpr.id] = checker.contextualize(
        type: receiverType, from: useSite, system: &system)
      desugared = MemberDeclRefExpr(base: receiverExpr, decl: decl, range: expr.range)
    } else {
      // Rewrite as a direct declaration reference.
      desugared = DeclRefExpr(decl: decl, type: .unresolved, range: expr.range)
    }

    // Contextualize the declaration's type in case it's generic.
    let type = checker.contextualize(
      type: checker.realize(typeOf: decl), from: useSite, system: &system)

    // Erase passing concentions from the type of parameter declaration references.
    if let type = type as? FunParamType {
      assert(decl is FunParamDecl)
      checker.exprTypes[desugared.id] = type.rawType
    } else {
      checker.exprTypes[desugared.id] = type
    }

    return desugared
  }

}
