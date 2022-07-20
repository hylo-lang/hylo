import Utils

/// Val's type checker.
public struct TypeChecker {

  /// The AST containing the modules being type checked.
  public internal(set) var ast: AST

  /// The scope hierarchy of the AST.
  public internal(set) var scopeHierarchy: ScopeHierarchy

  /// The diagnostics of the type errors.
  public internal(set) var diagnostics: Set<Diagnostic> = []

  /// The overarching type of each declaration.
  public private(set) var declTypes = DeclMap<Type>()

  /// The type of each expression.
  public private(set) var exprTypes = ExprMap<Type>()

  /// A table mapping name expressions to referred declarations.
  public internal(set) var referredDecls: [NodeID<NameExpr>: DeclRef] = [:]

  /// Indicates whether the type checker is processing the standard library.
  public var isProcessingStandardLibrary = false

  /// The set of lambda expressions whose declarations are pending type checking.
  var pendingLambdas: [NodeID<LambdaExpr>] = []

  /// Creates a new type checker for the specified AST.
  ///
  /// - Note: `ast` is stored in the type checker and mutated throughout type checking (e.g., to
  ///   insert synthesized declarations).
  public init(ast: AST) {
    self.ast = ast
    self.scopeHierarchy = ast.scopeHierarchy()
  }

  /// Type checks the AST.
  ///
  /// - Returns: A typed program if type checking succeeded. Otherwise, returns `nil`.
  public mutating func run() -> TypedProgram? {
    for module in ast.modules {
      guard check(module: module) else { return nil }
    }

    return TypedProgram(
      ast: ast,
      scopeHierarchy: scopeHierarchy,
      declTypes: declTypes,
      exprTypes: exprTypes,
      referredDecls: referredDecls)
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
        case .type(let a):
          return .type(canonicalize(type: a))
        case .value:
          fatalError("not implemented")
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
      return .union(UnionType(Set(t.elements.map(canonicalize(type:)))))

    default:
      unreachable()
    }
  }

  /// Returns the canonical form of `constraint`.
  public func canonicalize(constraint: Constraint) -> Constraint {
    var canonical = constraint
    canonical.modifyTypes({ type in
      type = canonicalize(type: type)
      return true
    })
    return canonical
  }

  /// Returns the set of traits to which `type` conforms in `scope`.
  ///
  /// - Note: If `type` is a trait, it is always contained in the returned set.
  mutating func conformedTraits<S: ScopeID>(of type: Type, inScope scope: S) -> Set<TraitType>? {
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

    case .product(let t):
      let decl = ast[t.decl]
      let parentScope = scopeHierarchy.container[t.decl]!
      guard let traits = realize(conformances: decl.conformances, inScope: parentScope)
        else { return nil }

      for trait in traits {
        guard let bases = conformedTraits(of: .trait(trait), inScope: parentScope)
          else { return nil }
        result.formUnion(bases)
      }

    case .trait(let t):
      // Gather the conformances defined at declaration.
      guard var work = realize(
        conformances: ast[t.decl].refinements,
        inScope: scopeHierarchy.container[t.decl]!)
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
      let parentScope = scopeHierarchy.container[i]!
      guard let traits = realize(conformances: decl.conformances, inScope: parentScope)
        else { return nil }

      for trait in traits {
        guard let bases = conformedTraits(of: .trait(trait), inScope: parentScope)
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

  /// The bindings whose initializers are being currently visited.
  private(set) var bindingsUnderChecking: DeclSet = []

  /// Processed all pending type checking requests and returns whether that succeeded.
  mutating func checkPending() -> Bool {
    var success = true

    while let id = pendingLambdas.popLast() {
      if case .lambda(let declType) = exprTypes[id],
         !declType.flags.contains(.hasError)
      {
        // Reify the type of the underlying declaration.
        declTypes[ast[id].decl] = .lambda(declType)
        let parameters = ast[ast[id].decl].parameters
        for i in 0 ..< parameters.count {
          declTypes[parameters[i]] = declType.inputs[i].type
        }

        // Type check the declaration.
        success = check(fun: ast[id].decl) && success
      } else {
        success = false
      }
    }

    return success
  }

  /// Type checks the specified module and returns whether that succeeded.
  ///
  /// - Requires: `id` is a valid ID in the type checker's AST.
  public mutating func check(module id: NodeID<ModuleDecl>) -> Bool {
    // Build the type of the module.
    declTypes[id] = .module(ModuleType(decl: id, ast: ast))

    // Type check the declarations in the module.
    var success = true
    for decl in ast[id].members {
      success = check(decl: decl) && success
    }

    // Process pending requests.
    return checkPending() && success
  }

  /// Type checks the specified declaration and returns whether that succeeded.
  private mutating func check<T: DeclID>(decl id: T) -> Bool {
    switch id.kind {
    case .associatedTypeDecl:
      return check(associatedType: NodeID(converting: id)!)
    case .associatedValueDecl:
      return check(associatedValue: NodeID(converting: id)!)
    case .bindingDecl:
      return check(binding: NodeID(converting: id)!)
    case .funDecl:
      return check(fun: NodeID(converting: id)!)
    case .methodImplDecl:
      return check(fun: NodeID(converting: scopeHierarchy.container[id]!)!)
    case .operatorDecl:
      return check(operator: NodeID(converting: id)!)
    case .productTypeDecl:
      return check(productType: NodeID(converting: id)!)
    case .subscriptDecl:
      return check(subscript: NodeID(converting: id)!)
    case .traitDecl:
      return check(trait: NodeID(converting: id)!)
    case .typeAliasDecl:
      return check(typeAlias: NodeID(converting: id)!)
    default:
      unreachable("unexpected declaration")
    }
  }

  private mutating func check(associatedType: NodeID<AssociatedTypeDecl>) -> Bool {
    return true
  }

  private mutating func check(associatedValue: NodeID<AssociatedValueDecl>) -> Bool {
    return true
  }

  /// - Note: Method is internal because it may be called during constraint generation.
  mutating func check(binding id: NodeID<BindingDecl>) -> Bool {
    defer { assert(declTypes[id] != nil) }

    // Note: binding declarations do not undergo type realization.
    switch declRequests[id] {
    case nil:
      declRequests[id] = .typeCheckingStarted
    case .typeCheckingStarted:
      diagnostics.insert(.circularDependency(range: ast.ranges[id]))
      return false
    case .success:
      return true
    case .failure:
      return false
    default:
      unreachable()
    }

    let scope = scopeHierarchy.container[AnyDeclID(id)]!
    let pattern = ast[id].pattern
    guard var shape = infer(pattern: pattern, inScope: scope) else {
      declTypes[id] = .error(ErrorType())
      declRequests[id] = .failure
      return false
    }

    // Type check the initializer, if any.
    var success = true
    if let initializer = ast[id].initializer {
      // The type of the initializer may be a subtype of the pattern's.
      let initializerType = Type.variable(TypeVariable(node: initializer.base))
      shape.constraints.append(LocatableConstraint(
        .equalityOrSubtyping(l: initializerType, r: shape.type),
        node: AnyNodeID(id),
        cause: .initialization))

      // Infer the type of the initializer
      let names = ast.names(in: ast[id].pattern).map({ AnyDeclID(ast[$0.pattern].decl) })
      bindingsUnderChecking.formUnion(names)
      let solution = infer(
        expr: initializer,
        inferredType: initializerType,
        expectedType: shape.type,
        inScope: scope,
        constraints: shape.constraints)
      bindingsUnderChecking.subtract(names)

      // TODO: Complete underspecified generic signatures

      success = solution.diagnostics.isEmpty
      shape.type = solution.reify(shape.type, withVariables: .substituteByError)

      // Assign the variable declarations in the pattern to their type
      for decl in shape.decls {
        declTypes[decl] = solution.reify(declTypes[decl]!, withVariables: .substituteByError)
        declRequests[decl] = success ? .success : .failure
      }
    } else if ast[ast[id].pattern].annotation == nil {
      let pattern = ast[ast[id].pattern]
      diagnostics.insert(.missingTypeAnnotation(range: ast.ranges[pattern.subpattern]))
      success = false
    }

    if success {
      assert(!shape.type[.hasVariable])
      declTypes[id] = shape.type
      declRequests[id] = .success
      return true
    } else {
      declTypes[id] = .error(ErrorType())
      declRequests[id] = .failure
      return false
    }
  }

  private mutating func check(conformance: ConformanceDecl) -> Bool {
    fatalError("not implemented")
  }

  private mutating func check(extension: ExtensionDecl) -> Bool {
    fatalError("not implemented")
  }

  /// Type checks the specified function declaration and returns whether that succeeded.
  ///
  /// The type of the declaration must be realizable from type annotations alone or the declaration
  /// the declaration must be realized and its inferred type must be stored in `declTyes`. Hence,
  /// the method must not be called on the underlying declaration of a lambda or async expression
  /// before the type of that declaration has been fully inferred.
  ///
  /// - SeeAlso: `checkPending`
  private mutating func check(fun id: NodeID<FunDecl>) -> Bool {
    _check(decl: id, { (this, id) in this._check(fun: id) })
  }

  private mutating func _check(fun id: NodeID<FunDecl>) -> Bool {
    var decl = ast[id]

    // Memberwize initializers always type check.
    if decl.introducer.value == .memberwiseInit {
      return true
    }

    // Type check the generic constraints of the declaration.
    var success = environment(ofGenericDecl: id) != nil

    // Type check the parameters.
    var names: Set<String> = []
    for parameterID in decl.parameters {
      let parameter = ast[parameterID]
      let parameterType = ParameterType(converting: declTypes[parameterID]!) ?? unreachable()

      // Check for duplicate parameter names.
      if !names.insert(parameter.name).inserted {
        diagnostics.insert(.duplicateParameterName(parameter.name, range: ast.ranges[parameterID]))
        declRequests[parameterID] = .failure
        success = false
        continue
      }

      // Type check the default value, if any.
      if let defaultValue = parameter.defaultValue {
        let defaultValueType = Type.variable(TypeVariable(node: defaultValue.base))
        let constraints = [
          LocatableConstraint(
            .parameter(l: defaultValueType, r: .parameter(parameterType)),
            node: AnyNodeID(parameterID),
            cause: .callArgument)
        ]

        let solution = infer(
          expr: defaultValue,
          inferredType: defaultValueType,
          expectedType: parameterType.bareType,
          inScope: AnyScopeID(id),
          constraints: constraints)

        if !solution.diagnostics.isEmpty {
          declRequests[parameterID] = .failure
          success = false
          continue
        }
      }

      // The parameter is checked.
      declRequests[parameterID] = .success
    }

    // Synthesize the receiver parameter if necessary.
    if case .bundle(let impls) = decl.body?.value {
      guard case .method(let type) = declTypes[id]! else { unreachable() }
      let bareType = type.receiver

      for j in impls {
        // Create the implicit parameter declaration.
        assert(ast[j].receiver == nil)
        let param = ast.insert(ParameterDecl(identifier: SourceRepresentable(value: "self")))
        scopeHierarchy.insert(decl: param, into: AnyScopeID(j))
        ast[j].receiver = param

        // Set its type.
        let convention: PassingConvention
        switch ast[j].introducer.value {
        case .let   : convention = .let
        case .inout : convention = .inout
        case .sink  : convention = .sink
        }
        declTypes[param] = .parameter(ParameterType(convention: convention, bareType: bareType))
        declRequests[param] = .success
      }
    } else if !decl.isStatic && scopeHierarchy.isMember(decl: id) {
      // Create the implicit parameter declaration.
      assert(!decl.implicitParameterDecls.contains(where: { $0.0 == "self" }))
      let param = ast.insert(ParameterDecl(identifier: SourceRepresentable(value: "self")))
      scopeHierarchy.insert(decl: param, into: AnyScopeID(id))
      ast[id].implicitParameterDecls.append(("self", AnyDeclID(param)))
      decl = ast[id]

      // Set its type.
      let type = LambdaType(converting: declTypes[id]!)!
      if decl.introducer.value == .`init` {
        // The receiver of an initializer is its first parameter.
        declTypes[param] = type.inputs[0].type
      } else if case .projection(let type) = type.captures.first?.type {
        // `let` and `inout` methods capture a projection of their receiver.
        let convention: PassingConvention
        switch type.capability {
        case .let   : convention = .let
        case .inout : convention = .inout
        case .set, .yielded:
          unreachable()
        }
        declTypes[param] = .parameter(ParameterType(convention: convention, bareType: type.base))
      } else {
        // `sink` methods capture their receiver.
        assert(decl.isSink)
        declTypes[param] = .parameter(ParameterType(convention: .sink, bareType: type.environment))
      }

      declRequests[param] = .success
    }

    // Retrieve the output type.
    let output: Type
    switch declTypes[id]! {
    case .lambda(let callable):
      output = callable.output.skolemized
    case .method(let callable):
      output = callable.output.skolemized
    default:
      unreachable()
    }

    // Type check the body of the function, if any.
    switch decl.body?.value {
    case .block(let stmt):
      success = check(brace: stmt) && success

    case .expr(let expr):
      // No need to type check the functon's body if it's been used to infer the return type.
      if (decl.output == nil) && decl.isInExprContext { break }
      success = (infer(expr: expr, expectedType: output, inScope: id) != nil) && success

    case .bundle(let impls):
      for j in impls {
        switch ast[j].body {
        case .expr(let expr):
          let expectedType: Type = ast[j].introducer.value == .inout
            ? .unit
            : output
          success = (infer(expr: expr, expectedType: expectedType, inScope: j) != nil) && success

        case .block(let stmt):
          success = check(brace: stmt) && success

        case nil:
          // Function without a body must be a requirement.
          assert(scopeHierarchy.container[id]?.kind == .traitDecl, "unexpected method requirement")
        }
      }

    case nil:
      // Function without a body must be a requirement.
      assert(scopeHierarchy.container[id]?.kind == .traitDecl, "unexpected method requirement")
    }

    return success
  }

  private mutating func check(genericTypeParam: GenericTypeParamDecl) -> Bool {
    fatalError("not implemented")
  }

  private mutating func check(genericValueParam: GenericValueParamDecl) -> Bool {
    fatalError("not implemented")
  }

  private mutating func check(methodImpl: MethodImplDecl) -> Bool {
    fatalError("not implemented")
  }

  private mutating func check(namespace: NamespaceDecl) -> Bool {
    fatalError("not implemented")
  }

  private mutating func check(parameter: ParameterDecl) -> Bool {
    fatalError("not implemented")
  }

  private mutating func check(operator id: NodeID<OperatorDecl>) -> Bool {
    guard let module = NodeID<ModuleDecl>(converting: scopeHierarchy.container[id]!) else {
      diagnostics.insert(.nestedOperatorDeclaration(range: ast.ranges[id]))
      return false
    }

    // Look for duplicate operator declaration.
    for decl in ast[module].members where decl.kind == .operatorDecl {
      let oper = NodeID<OperatorDecl>(unsafeRawValue: decl.rawValue)
      if oper != id,
         ast[oper].notation.value == ast[id].notation.value,
         ast[oper].name.value == ast[id].name.value
      {
        diagnostics.insert(.duplicateOperatorDeclaration(ast[id].name.value, range: ast.ranges[id]))
        return false
      }
    }

    return true
  }

  private mutating func check(productType id: NodeID<ProductTypeDecl>) -> Bool {
    _check(decl: id, { (this, id) in this._check(productType: id) })
  }

  private mutating func _check(productType id: NodeID<ProductTypeDecl>) -> Bool {
    // Type check the generic constraints of the declaration.
    var success = environment(ofGenericDecl: id) != nil

    // Type check the type's direct members.
    for j in ast[id].members {
      success = check(decl: j) && success
    }

    // Synthesize the memberwise initializer if necessary.
    success = check(fun: ast.memberwiseInitDecl(of: id, updating: &scopeHierarchy)) && success

    // Type check extending declarations.
    let type = declTypes[id]!
    for j in extendingDecls(of: type, exposedTo: scopeHierarchy.container[id]!) {
      success = check(decl: j) && success
    }

    // TODO: Check the conformances

    return success
  }

  private mutating func check(subscript id: NodeID<SubscriptDecl>) -> Bool {
    _check(decl: id, { (this, id) in
      // TODO: Implement me
      true
    })
  }

  private mutating func check(subscriptImpl: SubscriptImplDecl) -> Bool {
    fatalError("not implemented")
  }

  private mutating func check(trait id: NodeID<TraitDecl>) -> Bool {
    _check(decl: id, { (this, id) in this._check(trait: id) })
  }

  private mutating func _check(trait id: NodeID<TraitDecl>) -> Bool {
    // Type check the generic constraints of the declaration.
    var success = environment(ofTraitDecl: id) != nil

    // Type check the type's direct members.
    for j in ast[id].members {
      success = check(decl: j) && success
    }

    // Type check extending declarations.
    let type = declTypes[id]!
    for j in extendingDecls(of: type, exposedTo: scopeHierarchy.container[id]!) {
      success = check(decl: j) && success
    }

    // TODO: Check the conformances

    return success
  }

  private mutating func check(typeAlias id: NodeID<TypeAliasDecl>) -> Bool {
    _check(decl: id, { (this, id) in this._check(typeAlias: id) })
  }

  private mutating func _check(typeAlias id: NodeID<TypeAliasDecl>) -> Bool {
    // Realize the subject of the declaration.
    let subject: Type
    switch ast[id].body.value {
    case .typeExpr(let j):
      if let s = realize(j, inScope: AnyScopeID(id)) {
        subject = s
      } else {
        return false
      }

    case.union:
      fatalError("not implemented")
    }

    // Type-check the generic clause of the declaration.
    var success = environment(ofGenericDecl: id) != nil

    // Type check extending declarations.
    for j in extendingDecls(of: subject, exposedTo: scopeHierarchy.container[id]!) {
      success = check(decl: j) && success
    }

    // TODO: Check the conformances

    return success
  }

  private mutating func check(`var`: VarDecl) -> Bool {
    fatalError("not implemented")
  }

  /// Returns whether `decl` is well-typed from the cache, or calls `action` to type check it
  /// and caches the result before returning it.
  private mutating func _check<T: DeclID>(
    decl id: T,
    _ action: (inout Self, T) -> Bool
  ) -> Bool {
    // Check if a type checking request has already been received.
    while true {
      switch declRequests[id] {
      case nil:
        /// The the overarching type of the declaration is available after type realization.
        defer { assert(declRequests[id] != nil) }

        // Realize the type of the declaration before starting type checking.
        if realize(decl: id).isError {
          // Type checking fails if type realization did.
          declRequests[id] = .failure
          return false
        } else {
          // Note: Because the type realization of certain declarations may escalate to type
          // checking perform type checking, we should re-check the status of the request.
          continue
        }

      case .typeRealizationCompleted:
        declRequests[id] = .typeCheckingStarted

      case .typeRealizationStarted, .typeCheckingStarted:
        // Note: The request status will be updated when the request that caused the circular
        // dependency handles the failure.
        diagnostics.insert(.circularDependency(range: ast.ranges[id]))
        return false

      case .success:
        return true

      case .failure:
        return false
      }

      break
    }

    // Process the request.
    let success = action(&self, id)

    // Update the request status.
    declRequests[id] = success ? .success : .failure
    return success
  }

  /// Type checks the specified statement and returns whether that succeeded.
  private mutating func check<T: StmtID, S: ScopeID>(
    stmt id: T,
    inScope lexicalContext: S
  ) -> Bool {
    switch id.kind {
    case .braceStmt:
      return check(brace: NodeID(converting: id)!)

    case .exprStmt:
      let stmt = ast[NodeID<ExprStmt>(converting: id)!]
      if let type = infer(expr: stmt.expr, inScope: lexicalContext) {
        // Issue a warning if the type of the expression isn't unit.
        if type != .unit {
          diagnostics.insert(.unusedResult(ofType: type, range: ast.ranges[stmt.expr]))
        }
        return true
      } else {
        // Type inference/checking failed.
        return false
      }

    case .declStmt:
      return check(decl: ast[NodeID<DeclStmt>(converting: id)!].decl)

    case .discardStmt:
      let stmt = ast[NodeID<DiscardStmt>(converting: id)!]
      return infer(expr: stmt.expr, inScope: lexicalContext) != nil

    case .returnStmt:
      return check(return: NodeID(converting: id)!, inScope: lexicalContext)

    default:
      unreachable("unexpected statement")
    }
  }

  /// - Note: Method is internal because it may be called during constraint generation.
  mutating func check(brace id: NodeID<BraceStmt>) -> Bool {
    var success = true
    for stmt in ast[id].stmts {
      success = check(stmt: stmt, inScope: id) && success
    }
    return success
  }

  private mutating func check<S: ScopeID>(
    return id: NodeID<ReturnStmt>,
    inScope lexicalContext: S
  ) -> Bool {
    // Retrieve the function in which the return statement is contained to determine the expected
    // type of the return value.
    let funDecl = NodeID<FunDecl>(
      unsafeRawValue: scopeHierarchy
        .scopesToRoot(from: lexicalContext)
        .first(where: { $0.kind == .funDecl })!.rawValue)

    let expectedReturnType: Type
    switch declTypes[funDecl]! {
    case .lambda(let callable):
      expectedReturnType = callable.output.skolemized
    case .method(let callable):
      expectedReturnType = callable.output.skolemized
    default:
      unreachable()
    }

    if let returnValue = ast[id].value {
      // The type of the return value must be subtype of the expected return type.
      let inferredReturnType = Type.variable(TypeVariable(node: returnValue.base))
      let c = LocatableConstraint(
        .equalityOrSubtyping(l: inferredReturnType, r: expectedReturnType),
        node: returnValue.base,
        cause: .return)
      let solution = infer(
        expr: returnValue,
        inferredType: inferredReturnType,
        expectedType: expectedReturnType,
        inScope: AnyScopeID(lexicalContext),
        constraints: [c])
      return solution.diagnostics.isEmpty
    } else if expectedReturnType != .unit {
      diagnostics.insert(.missingReturnValue(range: ast.ranges[id]))
      return false
    } else {
      return true
    }
  }

  /// Returns the generic environment defined by `id`, or `nil` if it is ill-formed.
  ///
  /// - Requires: `i.kind <= .genericScope`
  private mutating func environment<T: NodeIDProtocol>(of id: T) -> GenericEnvironment? {
    switch id.kind {
    case .funDecl:
      return environment(ofGenericDecl: NodeID<FunDecl>(converting: id)!)
    case .productTypeDecl:
      return environment(ofGenericDecl: NodeID<ProductTypeDecl>(converting: id)!)
    case .subscriptDecl:
      return environment(ofGenericDecl: NodeID<SubscriptDecl>(converting: id)!)
    case .typeAliasDecl:
      return environment(ofGenericDecl: NodeID<TypeAliasDecl>(converting: id)!)
    case .traitDecl:
      return environment(ofTraitDecl: NodeID(converting: id)!)
    default:
      unreachable("unexpected scope")
    }
  }

  /// Returns the generic environment defined by `id`, or `nil` if it is ill-formed.
  private mutating func environment<T: GenericDecl>(
    ofGenericDecl id: NodeID<T>
  ) -> GenericEnvironment? {
    switch environments[id] {
    case .done(let e):
      return e
    case .inProgress:
      fatalError("circular dependency")
    case nil:
      environments[id] = .inProgress
    }

    // Nothing to do if the declaration has no generic clause.
    guard let clause = ast[id].genericClause?.value else {
      let e = GenericEnvironment(decl: id, constraints: [], into: &self)
      environments[id] = .done(e)
      return e
    }

    let declScope = AnyScopeID(converting: id)!
    let parentScope = scopeHierarchy.parent[declScope]!
    var success = true
    var constraints: [Constraint] = []

    // Realize the traits in the conformance lists of each generic parameter.
    for case .type(let j) in clause.parameters {
      // Realize the generic type parameter.
      guard let lhs = realize(decl: j).proper else {
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
      let e = GenericEnvironment(decl: id, constraints: constraints, into: &self)
      environments[id] = .done(e)
      return e
    } else {
      environments[id] = .done(nil)
      return nil
    }
  }

  /// Returns the generic environment defined by `i`, or `nil` if it is ill-formed.
  private mutating func environment<T: TypeExtendingDecl>(
    ofTypeExtendingDecl id: NodeID<T>
  ) -> GenericEnvironment? {
    switch environments[id] {
    case .done(let e):
      return e
    case .inProgress:
      fatalError("circular dependency")
    case nil:
      environments[id] = .inProgress
    }

    let scope = AnyScopeID(id)
    var success = true
    var constraints: [Constraint] = []

    // Evaluate the constraint expressions of the associated type's where clause.
    if let whereClause = ast[id].whereClause?.value {
      for expr in whereClause.constraints {
        if let constraint = eval(constraintExpr: expr, inScope: scope) {
          constraints.append(constraint)
        } else {
          success = false
        }
      }
    }

    if success {
      let e = GenericEnvironment(decl: id, constraints: constraints, into: &self)
      environments[id] = .done(e)
      return e
    } else {
      environments[id] = .done(nil)
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

    // Collect and type check the constraints defined on associated types and values.
    for member in ast[i].members {
      switch member.kind {
      case .associatedTypeDecl:
        success = associatedConstraints(
          ofType: NodeID(converting: member)!,
          ofTrait: i,
          into: &constraints) && success

      case .associatedValueDecl:
        success = associatedConstraints(
          ofValue: NodeID(converting: member)!,
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
    guard case .trait(let trait) = declTypes[i]! else { unreachable() }
    constraints.append(.conformance(l: .genericTypeParam(selfType), traits: [trait]))

    let e = GenericEnvironment(decl: i, constraints: constraints, into: &self)
    environments[i] = .done(e)
    return e
  }

  // Evaluates the constraints declared in `associatedType`, stores them in `constraints` and
  // returns whether they are all well-formed.
  private mutating func associatedConstraints(
    ofType associatedType: NodeID<AssociatedTypeDecl>,
    ofTrait trait: NodeID<TraitDecl>,
    into constraints: inout [Constraint]
  ) -> Bool {
    // Realize the generic type parameter corresponding to the associated type.
    let lhs = realize(decl: associatedType)
    if lhs.isError { return false }
    assert(lhs.base is GenericTypeParamType)

    // Synthesize the sugared conformance constraint, if any.
    let list = ast[associatedType].conformances
    guard let traits = realize(conformances: list, inScope: AnyScopeID(trait))
      else { return false }
    if !traits.isEmpty {
      constraints.append(.conformance(l: lhs, traits: traits))
    }

    // Evaluate the constraint expressions of the associated type's where clause.
    var success = true
    if let whereClause = ast[associatedType].whereClause?.value {
      for expr in whereClause.constraints {
        if let constraint = eval(constraintExpr: expr, inScope: AnyScopeID(trait)) {
          constraints.append(constraint)
        } else {
          success = false
        }
      }
    }

    return success
  }

  // Evaluates the constraints declared in `associatedValue`, stores them in `constraints` and
  // returns whether they are all well-formed.
  private mutating func associatedConstraints(
    ofValue associatedValue: NodeID<AssociatedValueDecl>,
    ofTrait trait: NodeID<TraitDecl>,
    into constraints: inout [Constraint]
  ) -> Bool {
    // Realize the generic type parameter corresponding to the associated value.
    let lhs = realize(decl: associatedValue)
    if lhs.isError { return false }
    assert(lhs.base is GenericValueParamType)

    var success = true

    // Evaluate the constraint expressions of the associated value's where clause.
    if let whereClause = ast[associatedValue].whereClause?.value {
      for expr in whereClause.constraints {
        if let constraint = eval(constraintExpr: expr, inScope: AnyScopeID(trait)) {
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
    inScope scope: AnyScopeID
  ) -> Constraint? {
    switch expr.value {
    case .equality(let l, let r):
      guard let a = realize(name: l, inScope: scope) else { return nil }
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

    case .value(let e):
      // TODO: Symbolic execution
      return .value(e)
    }
  }

  // MARK: Type inference

  /// Infers and returns the type of `expr`, or `nil` if `expr` is ill-formed.
  public mutating func infer<S: ScopeID>(
    expr: AnyExprID,
    expectedType: Type? = nil,
    inScope scope: S
  ) -> Type? {
    let solution = infer(
      expr: expr,
      inferredType: nil,
      expectedType: expectedType,
      inScope: AnyScopeID(scope),
      constraints: [])

    if solution.diagnostics.isEmpty {
      return exprTypes[expr]!
    } else {
      return nil
    }
  }

  /// Infers the type of `expr` and returns the best solution found by the constraint solver.
  mutating func infer(
    expr: AnyExprID,
    inferredType: Type?,
    expectedType: Type?,
    inScope scope: AnyScopeID,
    constraints: [LocatableConstraint]
  ) -> Solution {
    var constraints = constraints

    // Generate constraints.
    // Note: The constraint generator captures the ownership of `self`.
    var generator = ConstraintGenerator(checker: self, scope: scope)
    generator.inferredTypes[expr] = inferredType
    generator.expectedTypes[expr] = expectedType
    expr.accept(&generator)
    constraints.append(contentsOf: generator.constraints)

    // Solve the constraints.
    var solver = ConstraintSolver(
      checker: generator.checker.release(), scope: scope, fresh: constraints)
    var solution = solver.solve()!
    solution.diagnostics.append(contentsOf: generator.diagnostics)

    // Apply the solution.
    for (id, type) in generator.inferredTypes.storage {
      solver.checker.exprTypes[id] = solution.reify(type, withVariables: .keep)
    }
    for (name, ref) in solution.bindingAssumptions {
      solver.checker.referredDecls[name] = ref
    }

    // Puts `self` back in place.
    self = solver.checker.release()

    // Consume the solution's errors.
    diagnostics.formUnion(solution.diagnostics)

    return solution
  }

  /// Infers the type of `pattern`, generates the type constraints implied by the expressions it
  /// may contain, and returns the IDs of the variable declarations it contains.
  ///
  /// - Note: A `nil` return signals a failure to infer the type of the pattern.
  private mutating func infer<T: PatternID>(
    pattern: T,
    inScope scope: AnyScopeID
  ) -> (type: Type, constraints: [LocatableConstraint], decls: [NodeID<VarDecl>])? {
    var constraints: [LocatableConstraint] = []
    var decls: [NodeID<VarDecl>] = []
    if let type = _infer(
      pattern: pattern,
      expectedType: nil,
      inScope: scope,
      constraints: &constraints,
      decls: &decls)
    {
      return (type: type, constraints: constraints, decls: decls)
    } else {
      return nil
    }
  }

  private mutating func _infer<T: PatternID>(
    pattern: T,
    expectedType: Type?,
    inScope scope: AnyScopeID,
    constraints: inout [LocatableConstraint],
    decls: inout [NodeID<VarDecl>]
  ) -> Type? {
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

      return _infer(
        pattern: lhs.subpattern,
        expectedType: subpatternType,
        inScope: scope,
        constraints: &constraints,
        decls: &decls)

    case .expr:
      fatalError("not implemented")

    case .namePattern:
      let lhs = ast[NodeID<NamePattern>(converting: pattern)!]
      let type = expectedType ?? .variable(TypeVariable(node: AnyNodeID(lhs.decl)))
      decls.append(lhs.decl)
      declTypes[lhs.decl] = type
      declRequests[lhs.decl] = .typeRealizationCompleted
      return type

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
            if _infer(
              pattern: a.value.pattern,
              expectedType: b.type,
              inScope: scope,
              constraints: &constraints,
              decls: &decls) == nil
            { return nil }

            lLabels.append(a.value.label)
            rLabels.append(b.label)
          }

          // Check that labels match.
          if lLabels == rLabels {
            return expectedType
          } else {
            diagnostics.insert(.incompatibleLabels(
              found: lLabels, expected: rLabels, range: ast.ranges[pattern]))
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
          guard let type = _infer(
            pattern: element.value.pattern,
            expectedType: nil,
            inScope: scope,
            constraints: &constraints,
            decls: &decls)
          else { return nil }
          elements.append(TupleType.Element(label: element.value.label, type: type))
        }
        return .tuple(TupleType(elements))
      }

    case .wildcardPattern:
      return expectedType ?? .variable(TypeVariable())

    default:
      unreachable("unexpected pattern")
    }
  }

  // MARK: Name binding

  /// The result of a name lookup.
  public typealias DeclSet = Set<AnyDeclID>

  /// A lookup table.
  private typealias LookupTable = [String: DeclSet]

  private struct MemberLookupKey: Hashable {

    var type: Type

    var scope: AnyScopeID

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
  mutating func lookup(unqualified name: String, inScope scope: AnyScopeID) -> DeclSet {
    let origin = scope
    var root = scope

    var matches = DeclSet()
    for scope in scopeHierarchy.scopesToRoot(from: scope) {
      // Search for the name in the current scope.
      let newMatches = lookup(name, introducedInDeclSpaceOf: scope, inScope: origin)

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
  mutating func lookup<T: ScopeID>(
    _ name: String,
    introducedInDeclSpaceOf scope: T,
    inScope origin: AnyScopeID
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
  mutating func lookup(
    _ name: String,
    memberOf type: Type,
    inScope scope: AnyScopeID
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
      if name == "init" {
        matches.insert(AnyDeclID(ast.memberwiseInitDecl(of: t.decl, updating: &scopeHierarchy)))
      }

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
        // Associated type and value declarations are not inherited by conformance.
        matches.formUnion(newMatches.filter({
          $0.kind != .associatedTypeDecl && $0.kind != .associatedValueDecl
        }))
      }
    }

    return matches
  }

  /// Returns the declaration(s) of the specified operator.
  func lookup(
    operator operatorName: Identifier,
    notation: OperatorNotation,
    inScope scope: AnyScopeID
  ) -> [NodeID<OperatorDecl>] {
    func lookup(in module: NodeID<ModuleDecl>) -> NodeID<OperatorDecl>? {
      for decl in ast[module].members where decl.kind == .operatorDecl {
        let oper = NodeID<OperatorDecl>(unsafeRawValue: decl.rawValue)
        if (ast[oper].notation.value == notation) && (ast[oper].name.value == operatorName) {
          return oper
        }
      }
      return nil
    }

    let currentModule = scopeHierarchy.module(containing: scope)
    if let module = currentModule,
       let oper = lookup(in: module)
    {
      return [oper]
    }

    return ast.modules.compactMap({ (module) -> NodeID<OperatorDecl>? in
      if module == currentModule { return nil }
      return lookup(in: module)
    })
  }

  /// Returns the extending declarations of `type` exposed to `scope`.
  ///
  /// - Note: The declarations referred by the returned IDs conform to `TypeExtendingDecl`.
  private mutating func extendingDecls<S: ScopeID>(
    of type: Type,
    exposedTo scope: S
  ) -> [AnyDeclID] {
    var root = AnyScopeID(scope)
    var matches: [AnyDeclID] = []
    let canonicalType = canonicalize(type: type)

    func search(this: inout TypeChecker, inScope scope: AnyScopeID) {
      let decls = this.scopeHierarchy.containees[scope, default: []]
      for i in decls where i.kind == .conformanceDecl || i.kind == .extensionDecl {
        // Skip extending declarations that are being bound.
        guard this.extensionsUnderBinding.insert(i).inserted else { continue }
        defer { this.extensionsUnderBinding.remove(i) }

        // Bind the extension to the extended type.
        let subject = this.realize(decl: i)
        if subject.isError { continue }

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
      search(this: &self, inScope: AnyScopeID(module))
    }

    return matches
  }

  /// Returns the names and declarations introduced in `scope`.
  private func names<T: NodeIDProtocol>(introducedIn scope: T) -> LookupTable {
    guard let decls = scopeHierarchy.containees[scope] else { return [:] }

    var table: LookupTable = [:]
    for id in decls {
      switch id.kind {
      case .associatedValueDecl,
           .associatedTypeDecl,
           .genericValueParamDecl,
           .genericTypeParamDecl,
           .namespaceDecl,
           .parameterDecl,
           .productTypeDecl,
           .traitDecl,
           .typeAliasDecl,
           .varDecl:
        let name = (ast[id] as! SingleEntityDecl).name
        table[name, default: []].insert(AnyDeclID(id))

      case .bindingDecl:
        break

      case .funDecl:
        let decl = ast[NodeID<FunDecl>(converting: id)!]
        switch decl.introducer.value {
        case .memberwiseInit,
             .`init` where decl.body != nil:
          table["init", default: []].insert(AnyDeclID(id))

        case .deinit:
          assert(decl.body != nil)
          table["deinit", default: []].insert(AnyDeclID(id))

        default:
          guard let name = decl.identifier?.value else { continue }
          table[name, default: []].insert(AnyDeclID(id))
        }

      case .methodImplDecl:
        break

      case .operatorDecl:
        // Operator declarations are not considered during standard name lookup.
        break

      case .subscriptDecl:
        let decl = ast[NodeID<SubscriptDecl>(converting: id)!]
        let name = decl.identifier?.value ?? "[]"
        modifying(&table[name, default: []], { entries in
          for j in decl.impls {
            entries.insert(AnyDeclID(j))
          }
        })

      case .subscriptImplDecl:
        let decl = ast[NodeID<SubscriptDecl>(converting: scope)!]
        let name = decl.identifier?.value ?? "[]"
        table[name, default: []].insert(AnyDeclID(id))

      default:
        unreachable("unexpected declaration")
      }
    }

    // Note: Results should be memoized.
    return table
  }

  // MARK: Type realization

  /// Realizes and returns the type denoted by `expr` evaluated in `scope`.
  mutating func realize(_ expr: AnyTypeExprID, inScope scope: AnyScopeID) -> Type? {
    switch expr.kind {
    case .conformanceLensTypeExpr:
      return realize(conformanceLens: NodeID(converting: expr)!, inScope: scope)

    case .lambdaTypeExpr:
      return realize(lambda: NodeID(converting: expr)!, inScope: scope)

    case .nameTypeExpr:
      return realize(name: NodeID(converting: expr)!, inScope: scope)

    case .parameterTypeExpr:
      let id = NodeID<ParameterTypeExpr>(converting: expr)!
      diagnostics.insert(.illegalParameterConvention(
        ast[id].convention.value, range: ast[id].convention.range))
      return nil

    case .tupleTypeExpr:
      return realize(tuple: NodeID(converting: expr)!, inScope: scope)

    case .wildcardTypeExpr:
      return .variable(TypeVariable(node: expr.base))

    default:
      unreachable("unexpected type expression")
    }
  }

  /// Realizes and returns the type of `Self` in `scope`.
  ///
  /// - Note: This method does not issue diagnostics.
  public mutating func realizeSelfTypeExpr<T: ScopeID>(inScope scope: T) -> Type? {
    for scope in scopeHierarchy.scopesToRoot(from: scope) {
      switch scope.kind {
      case .traitDecl:
        let decl = NodeID<TraitDecl>(converting: scope)!
        return .genericTypeParam(GenericTypeParamType(decl: decl, ast: ast))

      case .productTypeDecl:
        // Synthesize unparametrized `Self`.
        let decl = NodeID<ProductTypeDecl>(converting: scope)!
        var type = Type.product(ProductType(decl: decl, ast: ast))

        // Synthesize arguments to generic parameters if necessary.
        if let parameters = ast[decl].genericClause?.value.parameters {
          let arguments = parameters.map({ (p) -> BoundGenericType.Argument in
            switch p {
            case .type(let p):
              return .type(.genericTypeParam(GenericTypeParamType(decl: p, ast: ast)))
            case .value:
              fatalError("not implemented")
            }
          })
          type = .boundGeneric(BoundGenericType(type, arguments: arguments))
        }

        return type

      case .typeAliasDecl:
        fatalError("not implemented")

      default:
        continue
      }
    }

    return nil
  }

  private mutating func realize(
    conformanceLens id: NodeID<ConformanceLensTypeExpr>,
    inScope scope: AnyScopeID
  ) -> Type? {
    let decl = ast[id]
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
    lambda id: NodeID<LambdaTypeExpr>,
    inScope scope: AnyScopeID
  ) -> Type? {
    // Realize the lambda's environment.
    let environment: Type
    if let env = ast[id].environment?.value {
      guard let ty = realize(env, inScope: scope) else { return nil }
      environment = ty
    } else {
      environment = .any
    }

    // Realize the lambda's parameters.
    var inputs: [CallableTypeParameter] = []
    for parameter in ast[id].parameters {
      guard let ty = realize(parameter: parameter.type, inScope: scope) else { return nil }
      inputs.append(CallableTypeParameter(label: parameter.label, type: ty))
    }

    // Realize the lambda's output.
    guard let output = realize(ast[id].output, inScope: scope) else { return nil }

    return .lambda(LambdaType(
      operatorProperty: ast[id].operatorProperty?.value,
      environment: environment,
      inputs: inputs,
      output: output))
  }

  private mutating func realize(
    name id: NodeID<NameTypeExpr>,
    inScope scope: AnyScopeID
  ) -> Type? {
    let identifier = ast[id].identifier
    var base: Type?

    if let j = ast[id].domain {
      // Resolve the domain.
      guard let domain = realize(j, inScope: scope) else { return nil }

      // Handle references to built-in types.
      if domain == .builtin(.module) {
        if let type = BuiltinType(identifier.value) {
          return .builtin(type)
        } else {
          diagnostics.insert(.noType(named: identifier.value, in: domain, range: identifier.range))
          return nil
        }
      }

      // Lookup for the name's identifier in the context of the domain.
      let matches = lookup(identifier.value, memberOf: domain, inScope: scope)

      // Realize the referred type.
      for match in matches where match.kind <= .typeDecl {
        if base != nil {
          diagnostics.insert(.ambiguousTypeReference(
            name: identifier.value, range: identifier.range))
          return nil
        }

        if match.kind == .associatedTypeDecl {
          let decl = NodeID<AssociatedTypeDecl>(converting: match)!
          switch domain {
          case .associated, .conformanceLens, .genericTypeParam:
            base = .associated(AssociatedType(decl: decl, domain: domain, ast: ast))
          default:
            diagnostics.insert(.invalidAssociatedTypeExpr(ast[decl].name, range: identifier.range))
            return nil
          }
        } else {
          base = realize(decl: match).proper
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

      if isProcessingStandardLibrary && (identifier.value == "Builtin") {
        return .builtin(.module)
      }

      // Search for the referred type declaration with an unqualified lookup.
      let matches = lookup(unqualified: identifier.value, inScope: scope)

      // Realize the referred type.
      for match in matches where match.kind <= .typeDecl {
        if base != nil {
          diagnostics.insert(
            .ambiguousTypeReference(name: identifier.value, range: identifier.range))
          return nil
        }

        if match.kind == .associatedTypeDecl {
          // Assume `Self` denotes the implicit generic parameter of a trait declaration, since
          // associated declarations cannot be looked up unqualified outside the scope of a trait
          // and its extensions.
          let domain = realizeSelfTypeExpr(inScope: scope)!
          let type = AssociatedType(decl: NodeID(converting: match)!, domain: domain, ast: ast)
          base = .associated(type)
        } else {
          base = realize(decl: match).proper
        }
      }

      if base == nil {
        diagnostics.insert(.noType(named: identifier.value, range: identifier.range))
        return nil
      }
    }

    // Evaluate the arguments of the referred type, if any.
    if ast[id].arguments.isEmpty {
      return base!
    } else {
      var arguments: [BoundGenericType.Argument] = []

      for a in ast[id].arguments {
        switch a.value {
        case .expr(let a):
          // TODO: Symbolic execution
          arguments.append(.value(a))

        case .type(let a):
          guard let type = realize(a, inScope: scope) else { return nil }
          arguments.append(.type(type))
        }
      }

      return .boundGeneric(BoundGenericType(base!, arguments: arguments))
    }
  }

  private mutating func realize(
    parameter id: NodeID<ParameterTypeExpr>,
    inScope scope: AnyScopeID
  ) -> Type? {
    guard let bareType = realize(ast[id].bareType, inScope: scope) else { return nil }
    return .parameter(ParameterType(convention: ast[id].convention.value, bareType: bareType))
  }

  private mutating func realize(
    tuple id: NodeID<TupleTypeExpr>,
    inScope scope: AnyScopeID
  ) -> Type? {
    var elements: [TupleType.Element] = []
    elements.reserveCapacity(ast[id].elements.count)

    for e in ast[id].elements {
      guard let type = realize(e.type, inScope: scope) else { return nil }
      elements.append(TupleType.Element(label: e.label?.value, type: type))
    }

    return .tuple(TupleType(elements))
  }

  /// Realizes and returns the traits of the specified conformance list, or `nil` if at least one
  /// of them is ill-formed.
  private mutating func realize(
    conformances: [NodeID<NameTypeExpr>],
    inScope scope: AnyScopeID
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
  mutating func realize<T: DeclID>(decl id: T) -> Type {
    switch id.kind {
    case .associatedTypeDecl,
         .genericTypeParamDecl:
      return _realize(decl: id, { (this, id) in
        .genericTypeParam(GenericTypeParamType(decl: id, ast: this.ast))
      })

    case .associatedValueDecl,
         .genericValueParamDecl:
      return _realize(decl: id, { (this, id) in
        .genericValueParam(GenericValueParamType(decl: id, ast: this.ast))
      })

    case .bindingDecl:
      return realize(bindingDecl: NodeID(converting: id)!)

    case .conformanceDecl,
         .extensionDecl:
      return _realize(decl: id, { (this, id) in
        let decl = this.ast[id] as! TypeExtendingDecl
        return this.realize(decl.subject, inScope: this.scopeHierarchy.container[id]!)
      })

    case .methodImplDecl:
      return realize(funDecl: NodeID(converting: scopeHierarchy.container[id]!)!)

    case .funDecl:
      return realize(funDecl: NodeID(converting: id)!)

    case .parameterDecl:
      return realize(parameterDecl: NodeID(converting: id)!)

    case .productTypeDecl:
      return _realize(decl: id, { (this, id) in
        .product(ProductType(decl: NodeID(converting: id)!, ast: this.ast))
      })

    case .subscriptDecl:
      return realize(subscriptDecl: NodeID(converting: id)!)

    case .traitDecl:
      return _realize(decl: id, { (this, id) in
        .trait(TraitType(decl: NodeID(converting: id)!, ast: this.ast))
      })

    case .typeAliasDecl:
      return _realize(decl: id, { (this, id) in
        .typeAlias(TypeAliasType(decl: NodeID(converting: id)!, ast: this.ast))
      })

    case .varDecl:
      let bindingDecl = scopeHierarchy.varToBinding[NodeID(converting: id)!]!
      let bindingType = realize(bindingDecl: bindingDecl)
      return bindingType.isError
        ? bindingType
        : declTypes[id]!

    default:
      unreachable("unexpected declaration")
    }
  }

  private mutating func realize(bindingDecl id: NodeID<BindingDecl>) -> Type {
    _ = check(binding: NodeID(converting: id)!)
    return declTypes[id]!
  }

  /// Returns the overarching type of the given function declaration.
  mutating func realize(funDecl id: NodeID<FunDecl>) -> Type {
    _realize(decl: id, { (this, id) in this._realize(funDecl: id) })
  }

  private mutating func _realize(funDecl id: NodeID<FunDecl>) -> Type {
    guard let parentScope = scopeHierarchy.container[id] else { unreachable() }

    // Handle memberwise initializers.
    if ast[id].introducer.value == .memberwiseInit {
      guard let productTypeDecl = NodeID<ProductTypeDecl>(converting: parentScope) else {
        diagnostics.insert(.illegalMemberwiseInit(range: ast[id].introducer.range))
        return .error(ErrorType())
      }

      if let lambda = memberwiseInitType(of: productTypeDecl) {
        return .lambda(lambda)
      } else {
        return .error(ErrorType())
      }
    }

    var decl = ast[id]
    let declScope = AnyScopeID(id)

    var implicitParameterDecls: [(name: String, decl: AnyDeclID)] = []
    var inputs: [CallableTypeParameter] = []
    var success = true

    // Realize the input types.
    for i in decl.parameters {
      declRequests[i] = .typeCheckingStarted

      if let annotation = ast[i].annotation {
        if let type = realize(parameter: annotation, inScope: declScope) {
          // The annotation may not omit generic arguments.
          if type[.hasVariable] {
            diagnostics.insert(.notEnoughContextToInferArguments(range: ast.ranges[annotation]))
            success = false
          }

          declTypes[i] = type
          declRequests[i] = .typeRealizationCompleted
          inputs.append(CallableTypeParameter(label: ast[i].label?.value, type: type))
        } else {
          declTypes[i] = .error(ErrorType())
          declRequests[i] = .failure
          success = false
        }
      } else if decl.isInExprContext {
        let parameterType = Type.variable(TypeVariable(node: AnyNodeID(i)))
        declTypes[i] = parameterType
        declRequests[i] = .typeRealizationCompleted
        inputs.append(CallableTypeParameter(label: ast[i].label?.value, type: parameterType))
      } else {
        declTypes[i] = .error(ErrorType())
        declRequests[i] = .failure
        diagnostics.insert(.missingTypeAnnotation(range: ast.ranges[i]))
        success = false
      }
    }

    // Collect explicit captures.
    var captures: [Type] = []
    var explictNames: Set<Name> = []
    for i in decl.explicitCaptures {
      let pattern = ast[i].pattern

      // Collect the names of the capture.
      for (_, name) in ast.names(in: pattern) {
        let stem = ast[ast[name].decl].name
        if explictNames.insert(Name(stem: stem)).inserted {
          implicitParameterDecls.append((name: stem, decl: AnyDeclID(ast[name].decl)))
        } else {
          diagnostics.insert(.duplicateCaptureName(stem, range: ast.ranges[ast[name].decl]))
          success = false
        }
      }

      // Realize the type of the capture.
      if let type = realize(bindingDecl: i).proper {
        switch ast[ast[i].pattern].introducer.value {
        case .let:
          captures.append(.projection(ProjectionType(.let, type)))
        case .inout:
          captures.append(.projection(ProjectionType(.inout, type)))
        case .sinklet, .var:
          captures.append(type)
        }
      } else {
        success = false
      }
    }

    // Bail out if parameters or captures could not be realized.
    if !success { return .error(ErrorType()) }

    // Collect implicit captures if the function's local.
    if scopeHierarchy.isLocal(decl: id, ast: ast) {
      var receiverCaptureIndex: Int? = nil
      var collector = CaptureCollector(ast: ast)
      let freeNames = collector.freeNames(in: id)

      for (name, uses) in freeNames {
        // Explicit captures are already accounted for.
        if explictNames.contains(name) { continue }

        // Resolve the name.
        let matches = lookup(unqualified: name.stem, inScope: parentScope)

        // If there is a single match, assume it's correct. Type checking will fail it it isn't.
        // If there are multiple match, attempt to filter them using the name's argument labels or
        // operator notation. If that fails, complain about an ambiguous implicit capture.
        let decl: AnyDeclID
        switch matches.count {
        case 0: continue
        case 1: decl = matches.first!
        default:
          fatalError("not implemented")
        }

        // Global declarations are not captured.
        if scopeHierarchy.isGlobal(decl: decl, ast: ast) { continue }

        // References to member declarations implicitly capture of their receiver.
        if scopeHierarchy.isMember(decl: decl) {
          // If the function refers to a member declaration, it must be nested in a type scope.
          let innermostTypeScope = scopeHierarchy
            .scopesToRoot(from: scopeHierarchy.parent[id]!)
            .first(where: { $0.kind <= .typeDecl })!

          // Ignore illegal implicit references to foreign receiver.
          if scopeHierarchy.isContained(innermostTypeScope, in: scopeHierarchy.parent[decl]!) {
            continue
          }

          if let i = receiverCaptureIndex {
            // Update the mutability of the capture if necessary.
            if uses.capability == .let { continue }
            guard case let .projection(p) = captures[i] else { unreachable() }
            captures[i] = .projection(ProjectionType(.inout, p.base))
          } else {
            // Capture the method's receiver.
            let captureType = realizeSelfTypeExpr(inScope: id)!
            receiverCaptureIndex = captures.count
            captures.append(.projection(ProjectionType(uses.capability, captureType)))
          }

          continue
        }

        // Capture-less local functions are not captured.
        if let funDecl = NodeID<FunDecl>(converting: decl) {
          guard case .lambda(let lambda) = realize(funDecl: funDecl) else { continue }
          if lambda.environment == .unit { continue }
        }

        // Other local declarations are captured.
        guard let captureType = realize(decl: decl).proper?.skolemized else { continue }
        captures.append(.projection(ProjectionType(uses.capability, captureType)))
        implicitParameterDecls.append((name.stem, decl))
      }
    }

    ast[id].implicitParameterDecls = implicitParameterDecls
    decl = ast[id]

    // Member declarations may not have captures.
    if scopeHierarchy.isMember(decl: id) && !captures.isEmpty {
      diagnostics.insert(.memberDeclHasCaptures(range: ast[id].introducer.range))
      return .error(ErrorType())
    }

    // Generic declarations may not have captures.
    if ast[id].genericClause != nil && !captures.isEmpty {
      diagnostics.insert(.genericDeclHasCaptures(range: ast[id].introducer.range))
      return .error(ErrorType())
    }

    // Realize the function's receiver if necessary.
    let isNonStaticMember = scopeHierarchy.isMember(decl: id) && !ast[id].isStatic
    var receiver: Type? = isNonStaticMember
      ? realizeSelfTypeExpr(inScope: scopeHierarchy.container[id]!)
      : nil

    // Handle initializers and deinitializers.
    switch ast[id].introducer.value {
    case .memberwiseInit:
      unreachable()

    case .`init`:
      // Initializers are global functions.
      let receiverParameter = CallableTypeParameter(
        label: "self",
        type: .parameter(ParameterType(convention: .set, bareType: receiver!)))
      inputs.insert(receiverParameter, at: 0)
      return .lambda(LambdaType(environment: .unit, inputs: inputs, output: .unit))

    case .deinit:
      // Deinitializers are sink methods.
      return .method(MethodType(
        capabilities: [.sink],
        receiver: receiver!,
        inputs: [],
        output: .unit))

    case .fun:
      // Realize the output type.
      let output: Type
      if let o = decl.output {
        // Use explicit return annotations.
        guard let type = realize(o, inScope: declScope) else { return .error(ErrorType()) }
        output = type
      } else if decl.isInExprContext {
        // Return types may be inferred in expression contexts.
        output = .variable(TypeVariable())
      } else {
        // Default to `()`
        output = .unit
      }

      if case .bundle(let impls) = ast[id].body?.value {
        // Create a method bundle.
        let capabilities = Set(impls.map({ ast[$0].introducer.value }))

        if capabilities.contains(.inout) && (output != receiver) {
          let range = decl.output.map({ ast.ranges[$0] }) ?? decl.introducer.range
          diagnostics.insert(.invalidInoutBundleReturnType(expected: receiver!, range: range))
          return .error(ErrorType())
        }

        return .method(MethodType(
          capabilities: Set(impls.map({ ast[$0].introducer.value })),
          receiver: receiver!,
          inputs: inputs,
          output: output))
      } else if isNonStaticMember {
        // Create a lambda bound to a receiver.
        let property: LambdaType.OperatorProperty?
        if ast[id].isInout {
          receiver = .tuple(TupleType(labelsAndTypes: [
            ("self", .projection(ProjectionType(.inout, receiver!)))
          ]))
          property = .inout
        } else if decl.isSink  {
          receiver = .tuple(TupleType(labelsAndTypes: [("self", receiver!)]))
          property = .sink
        } else {
          receiver = .tuple(TupleType(labelsAndTypes: [
            ("self", .projection(ProjectionType(.let, receiver!)))
          ]))
          property = nil
        }

        return .lambda(LambdaType(
          operatorProperty: property, environment: receiver!, inputs: inputs, output: output))
      } else {
        // Create a regular lambda.
        let environment = Type.tuple(TupleType(types: captures))

        // TODO: Determine if the lambda is mutating

        return .lambda(LambdaType(environment: environment, inputs: inputs, output: output))
      }
    }
  }

  /// Returns the overarching type of the specified parameter declaration.
  ///
  /// - Requires: The containing function or subscript declaration must have been realized.
  private mutating func realize(parameterDecl id : NodeID<ParameterDecl>) -> Type {
    switch declRequests[id] {
    case nil:
      preconditionFailure()

    case .typeRealizationStarted:
      diagnostics.insert(.circularDependency(range: ast.ranges[id]))
      return .error(ErrorType())

    case .typeRealizationCompleted, .typeCheckingStarted, .success, .failure:
      return declTypes[id]!
    }
  }

  private mutating func realize(subscriptDecl id: NodeID<SubscriptDecl>) -> Type {
    _realize(decl: id, { (this, id) in this._realize(subscriptDecl: id) })
  }

  private mutating func _realize(subscriptDecl id: NodeID<SubscriptDecl>) -> Type {
    let decl = ast[id]

    var inputs: [CallableTypeParameter] = []

    // Realize the input types.
    if decl.parameters != nil { fatalError("not implemented") }

    // Synthesize the receiver parameter, if necessary.
    // FIXME: Receiver is not a parameter
    let parent = scopeHierarchy.container[id] ?? unreachable()
    switch parent.kind {
    case .productTypeDecl,
         .traitDecl,
         .typeAliasDecl:
      let receiver = realizeSelfTypeExpr(inScope: parent)!
      inputs.insert(CallableTypeParameter(label: nil, type: receiver), at: 0)

    default:
      break
    }

    // Realize the ouput type an collect capabilities.
    guard let output = realize(decl.output, inScope: AnyScopeID(id)) else {
      return .error(ErrorType())
    }
    let capabilities = Set(decl.impls.map({ ast[$0].introducer.value }))

    return .subscript(SubscriptType(
      isProperty: decl.parameters == nil,
      capabilities: capabilities,
      inputs: inputs,
      output: output))
  }

  /// Returns the type of `decl` from the cache, or calls `action` to compute it and caches the
  /// result before returning it.
  private mutating func _realize<T: DeclID>(
    decl id: T,
    _ action: (inout Self, T) -> Type?
  ) -> Type {
    // Check if a type realization request has already been received.
    switch declRequests[id] {
    case nil:
      declRequests[id] = .typeRealizationStarted

    case .typeRealizationStarted:
      diagnostics.insert(.circularDependency(range: ast.ranges[id]))
      declRequests[id] = .failure
      declTypes[id] = .error(ErrorType())
      return declTypes[id]!

    case .typeRealizationCompleted, .typeCheckingStarted, .success, .failure:
      return declTypes[id]!
    }

    // Process the request.
    declTypes[id] = action(&self, id)

    // Update the request status.
    declRequests[id] = .typeRealizationCompleted
    return declTypes[id]!
  }

  /// Returns the type of `decl`'s memberwise initializer.
  private mutating func memberwiseInitType(of decl: NodeID<ProductTypeDecl>) -> LambdaType? {
    var inputs: [CallableTypeParameter] = []

    // Synthesize the receiver type.
    let receiver = realizeSelfTypeExpr(inScope: decl)!
    inputs.append(CallableTypeParameter(
      label: "self",
      type: .parameter(ParameterType(convention: .set, bareType: receiver))))

    // List and realize the type of all stored bindings.
    for m in ast[decl].members {
      guard let member = NodeID<BindingDecl>(converting: m) else { continue }
      if realize(bindingDecl: member).isError { return nil }

      for (_, name) in ast.names(in: ast[member].pattern) {
        let d = ast[name].decl
        inputs.append(CallableTypeParameter(
          label: ast[d].name,
          type: .parameter(ParameterType(convention: .sink, bareType: declTypes[d]!))))
      }
    }

    return LambdaType(environment: .unit, inputs: inputs, output: .unit)
  }

  // MARK: Type role determination

  /// Skolemizes `type`.
  func skolemize(type: Type) -> Type {
    return type.transform({ type in
      switch type {
      case .associated,
           .genericTypeParam:
        return .stepOver(.skolem(SkolemType(base: type)))

      case .genericValueParam:
        fatalError("not implemented")

      default:
        // Nothing to do if `type` isn't parameterized.
        if type[.hasGenericTypeParam] || type[.hasGenericValueParam] {
          return .stepInto(type)
        } else {
          return .stepOver(type)
        }
      }
    })
  }

  /// Opens `type`.
  func open(type: Type) -> (Type, [Constraint]) {
    var openedParameters: [Type: Type] = [:]

    let transformed = type.transform({ type in
      switch type {
      case .associated:
        fatalError("not implemented")

      case .genericTypeParam:
        if let opened = openedParameters[type] {
          // The parameter was already opened.
          return .stepOver(opened)
        } else {
          // Open the parameter.
          let opened = Type.variable(TypeVariable())
          openedParameters[type] = opened

          // TODO: Collect constraints

          return .stepOver(opened)
        }

      case .genericValueParam:
        fatalError("not implemented")

      default:
        // Nothing to do if `type` isn't parameterized.
        if type[.hasGenericTypeParam] || type[.hasGenericValueParam] {
          return .stepInto(type)
        } else {
          return .stepOver(type)
        }
      }
    })

    return (transformed, [])
  }

  /// Returns `type` contextualized in `scope` and the type constraints implied by that
  /// contextualization.
  ///
  /// Contextualization consists of substituting the generic parameters in a type by either skolems
  /// or open variables. Opened parameters carry the constraints defined by the generic environment
  /// in which they are opened.
  func contextualize<S: ScopeID>(type: Type, inScope scope: S) -> (Type, [Constraint]) {
    var openedParameters: [Type: Type] = [:]

    let transformed = type.transform({ type in
      switch type {
      case .associated:
        fatalError("not implemented")

      case .genericTypeParam(let base):
        // Identify the generic environment that introduces the parameter.
        let origin: AnyScopeID
        if base.decl.kind == .traitDecl {
          origin = AnyScopeID(converting: base.decl)!
        } else {
          origin = scopeHierarchy.container[base.decl]!
        }

        if scopeHierarchy.isContained(scope, in: origin) {
          // Skolemize.
          return .stepOver(.skolem(SkolemType(base: type)))
        } else if let opened = openedParameters[type] {
          // The parameter was already opened.
          return .stepOver(opened)
        } else {
          // Open the parameter.
          let opened = Type.variable(TypeVariable())
          openedParameters[type] = opened

          // TODO: Collect constraints

          return .stepOver(opened)
        }

      case .genericValueParam:
        fatalError("not implemented")

      default:
        // Nothing to do if `type` isn't parameterized.
        if type[.hasGenericTypeParam] || type[.hasGenericValueParam] {
          return .stepInto(type)
        } else {
          return .stepOver(type)
        }
      }
    })

    return (transformed, [])
  }

}
