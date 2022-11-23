import Utils

/// Val's type checker.
public struct TypeChecker {

  /// The program being type checked.
  public internal(set) var program: ScopedProgram

  /// The diagnostics of the type errors.
  public internal(set) var diagnostics: Set<Diagnostic> = []

  /// The overarching type of each declaration.
  public private(set) var declTypes = DeclProperty<Type>()

  /// The type of each expression.
  public private(set) var exprTypes = ExprProperty<Type>()

  /// A map from function and subscript declarations to their implicit captures.
  public private(set) var implicitCaptures = DeclProperty<[ImplicitCapture]>()

  /// A map from name expression to its referred declaration.
  public internal(set) var referredDecls: [NodeID<NameExpr>: DeclRef] = [:]

  /// A map from sequence expressions to their evaluation order.
  public internal(set) var foldedSequenceExprs: [NodeID<SequenceExpr>: FoldedSequenceExpr] = [:]

  /// Indicates whether the built-in symbols are visible.
  public var isBuiltinModuleVisible: Bool

  /// The set of lambda expressions whose declarations are pending type checking.
  public private(set) var pendingLambdas: [NodeID<LambdaExpr>] = []

  /// Creates a new type checker for the specified program.
  ///
  /// - Note: `program` is stored in the type checker and mutated throughout type checking (e.g.,
  ///   to insert synthesized declarations).
  public init(program: ScopedProgram, isBuiltinModuleVisible: Bool = false) {
    self.program = program
    self.isBuiltinModuleVisible = isBuiltinModuleVisible
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
        constraints: ConstraintSet(t.constraints.map(canonicalize(constraint:)))))

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
    canonical.modifyTypes({ (type) in type = canonicalize(type: type) })
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
      case GenericTypeParamDecl.self:
        let parameter = NodeID<GenericTypeParamDecl>(rawValue: t.decl.rawValue)
        guard let traits = realize(
          conformances: program.ast[parameter].conformances,
          inScope: program.scopeToParent[t.decl]!)
        else { return nil }
        result.formUnion(traits)

      case TraitDecl.self:
        let trait = TraitType(decl: NodeID(rawValue: t.decl.rawValue), ast: program.ast)
        return conformedTraits(of: .trait(trait), inScope: scope)

      default:
        break
      }

      // Gather conformances defined by conditional conformances/extensions.
      for scope in program.scopes(from: scope) where scope.kind.value is GenericScope.Type {
        guard let e = environment(of: scope) else { continue }
        result.formUnion(e.conformedTraits(of: type))
      }

    case .product(let t):
      let decl = program.ast[t.decl]
      let parentScope = program.declToScope[t.decl]!
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
        conformances: program.ast[t.decl].refinements,
        inScope: program.declToScope[t.decl]!)
      else { return nil }

      while let base = work.popFirst() {
        if base == t {
          diagnostics.insert(.diagnose(circularRefinementAt: program.ast[t.decl].identifier.range))
          return nil
        } else if result.insert(base).inserted {
          guard let traits = realize(
            conformances: program.ast[base.decl].refinements,
            inScope: program.scopeToParent[base.decl]!)
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
    for i in extendingDecls(of: type, exposedTo: scope) where i.kind == ConformanceDecl.self {
      let decl = program.ast[NodeID<ConformanceDecl>(rawValue: i.rawValue)]
      let parentScope = program.declToScope[i]!
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
    /// The checker is verifying whether the declaration is well-typed; its overarching type is
    /// available in `declTypes`. Initiating a new type checking request will cause a circular
    /// dependency error.
    case typeCheckingStarted

    /// Type checking succeeded.
    ///
    /// The declaration is well-typed; its overarching type is availabe in `declTypes`.
    case success

    /// Type realzation or type checking failed.
    ///
    /// If type realization succeeded, the overarching type of the declaration is available in
    /// `declTypes`. Otherwise, it is assigned to `nil`.
    case failure

  }

  /// A cache for type checking requests on declarations.
  private var declRequests = DeclProperty<RequestStatus>()

  /// A cache mapping generic declarations to their environment.
  private var environments = DeclProperty<MemoizationState<GenericEnvironment?>>()

  /// The bindings whose initializers are being currently visited.
  private var bindingsUnderChecking: DeclSet = []

  /// Inserts `expr` in the set of pending lambdas.
  mutating func deferTypeChecking(_ expr: NodeID<LambdaExpr>) {
    pendingLambdas.append(expr)
  }

  /// Processed all pending type checking requests and returns whether that succeeded.
  mutating func checkPending() -> Bool {
    var success = true

    while let id = pendingLambdas.popLast() {
      if case .lambda(let declType) = exprTypes[id],
         !declType.flags.contains(.hasError)
      {
        // Reify the type of the underlying declaration.
        declTypes[program.ast[id].decl] = .lambda(declType)
        let parameters = program.ast[program.ast[id].decl].parameters
        for i in 0 ..< parameters.count {
          declTypes[parameters[i]] = declType.inputs[i].type
        }

        // Type check the declaration.
        success = check(function: program.ast[id].decl) && success
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
    declTypes[id] = .module(ModuleType(decl: id, ast: program.ast))

    // Type check the declarations in the module.
    var success = true
    for decl in program.ast.topLevelDecls(id) {
      success = check(decl: decl) && success
    }

    // Process pending requests.
    return checkPending() && success
  }

  /// Type checks the specified declaration and returns whether that succeeded.
  private mutating func check<T: DeclID>(decl id: T) -> Bool {
    switch id.kind {
    case AssociatedTypeDecl.self:
      return check(associatedType: NodeID(rawValue: id.rawValue))
    case AssociatedValueDecl.self:
      return check(associatedValue: NodeID(rawValue: id.rawValue))
    case BindingDecl.self:
      return check(binding: NodeID(rawValue: id.rawValue))
    case ConformanceDecl.self:
      return check(conformance: NodeID(rawValue: id.rawValue))
    case FunctionDecl.self:
      return check(function: NodeID(rawValue: id.rawValue))
    case InitializerDecl.self:
      return check(initializer: NodeID(rawValue: id.rawValue))
    case MethodDecl.self:
      return check(method: NodeID(rawValue: id.rawValue))
    case MethodImplDecl.self:
      return check(method: NodeID(rawValue: program.declToScope[id]!.rawValue))
    case OperatorDecl.self:
      return check(operator: NodeID(rawValue: id.rawValue))
    case ProductTypeDecl.self:
      return check(productType: NodeID(rawValue: id.rawValue))
    case SubscriptDecl.self:
      return check(subscript: NodeID(rawValue: id.rawValue))
    case TraitDecl.self:
      return check(trait: NodeID(rawValue: id.rawValue))
    case TypeAliasDecl.self:
      return check(typeAlias: NodeID(rawValue: id.rawValue))
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

  private mutating func check(conformance: NodeID<ConformanceDecl>) -> Bool {
    // FIXME: implement me.
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
      diagnostics.insert(.diagnose(circularDependencyAt: program.ast.ranges[id]))
      return false
    case .success:
      return true
    case .failure:
      return false
    default:
      unreachable()
    }

    let scope = program.declToScope[AnyDeclID(id)]!
    let pattern = program.ast[id].pattern
    guard var shape = infer(pattern: pattern, inScope: scope) else {
      declTypes[id] = .error(ErrorType())
      declRequests[id] = .failure
      return false
    }

    // Type check the initializer, if any.
    var success = true
    if let initializer = program.ast[id].initializer {
      // The type of the initializer may be a subtype of the pattern's.
      let initializerType = Type.variable(TypeVariable(node: initializer.base))
      shape.constraints.append(
        equalityOrSubtypingConstraint(
          initializerType,
          shape.type,
          because: ConstraintCause(.initialization, at: program.ast.ranges[id])))

      // Infer the type of the initializer
      let names = program.ast.names(in: program.ast[id].pattern).map({ (name) in
        AnyDeclID(program.ast[name.pattern].decl)
      })

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
    } else if program.ast[program.ast[id].pattern].annotation == nil {
      unreachable("expected type annotation")
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
  private mutating func check(function id: NodeID<FunctionDecl>) -> Bool {
    _check(decl: id, { (this, id) in this._check(function: id) })
  }

  private mutating func _check(function id: NodeID<FunctionDecl>) -> Bool {
    // Type check the generic constraints.
    var success = environment(ofGenericDecl: id) != nil

    // Type check the parameters.
    var parameterNames: Set<String> = []
    for parameter in program.ast[id].parameters {
      success = check(parameter: parameter, siblingNames: &parameterNames) && success
    }

    // Set the type of the implicit receiver declaration if necessary.
    if program.isNonStaticMember(id) {
      let functionType = LambdaType(declTypes[id]!)!
      let receiverDecl = program.ast[id].receiver!

      if case .remote(let type) = functionType.captures.first?.type {
        // `let` and `inout` methods capture a projection of their receiver.
        let convention: PassingConvention
        switch type.capability {
        case .let   : convention = .let
        case .inout : convention = .inout
        case .set, .yielded:
          unreachable()
        }

        declTypes[receiverDecl] = .parameter(ParameterType(
          convention: convention,
          bareType: type.base))
      } else {
        // `sink` methods capture their receiver.
        assert(program.ast[id].isSink)
        declTypes[receiverDecl] = .parameter(ParameterType(
          convention: .sink,
          bareType: functionType.environment))
      }

      declRequests[receiverDecl] = .success
    }

    // Type check the body, if any.
    switch program.ast[id].body {
    case .block(let stmt):
      return check(brace: stmt) && success

    case .expr(let expr):
      // If `expr` has been used to infer the return type, there's no need to visit it again.
      if (program.ast[id].output == nil) && program.ast[id].isInExprContext { return success }

      // Otherwise, it's expected to have the realized return type.
      guard case .lambda(let type) = declTypes[id]! else { unreachable() }
      let inferredType = infer(expr: expr, expectedType: type.output.skolemized, inScope: id)
      return (inferredType != nil) && success

    case nil:
      // Requirements and FFIs can be without a body.
      if program.isRequirement(id) || program.ast[id].isFFI { return success }

      // Declaration requires a body.
      diagnostics.insert(.diagnose(declarationRequiresBodyAt: program.ast[id].introducerRange))
      return false
    }
  }

  private mutating func check(genericTypeParam: GenericTypeParamDecl) -> Bool {
    fatalError("not implemented")
  }

  private mutating func check(genericValueParam: GenericValueParamDecl) -> Bool {
    fatalError("not implemented")
  }

  private mutating func check(initializer id: NodeID<InitializerDecl>) -> Bool {
    _check(decl: id, { (this, id) in this._check(initializer: id) })
  }

  private mutating func _check(initializer id: NodeID<InitializerDecl>) -> Bool {
    // Memberwize initializers always type check.
    if program.ast[id].introducer.value == .memberwiseInit {
      return true
    }

    // The type of the declaration must have been realized.
    guard case .lambda(let type) = declTypes[id]! else { unreachable() }

    // Type check the generic constraints.
    var success = environment(ofGenericDecl: id) != nil

    // Type check the parameters.
    var parameterNames: Set<String> = []
    for parameter in program.ast[id].parameters {
      success = check(parameter: parameter, siblingNames: &parameterNames) && success
    }

    // Set the type of the implicit receiver declaration.
    // Note: the receiver of an initializer is its first parameter.
    declTypes[program.ast[id].receiver] = type.inputs[0].type
    declRequests[program.ast[id].receiver] = .success

    // Type check the body, if any.
    if let body = program.ast[id].body {
      return check(brace: body) && success
    } else if program.isRequirement(id) {
      return success
    } else {
      diagnostics.insert(.diagnose(declarationRequiresBodyAt: program.ast[id].introducer.range))
      return false
    }
  }

  private mutating func check(method id: NodeID<MethodDecl>) -> Bool {
    _check(decl: id, { (this, id) in this._check(method: id) })
  }

  private mutating func _check(method id: NodeID<MethodDecl>) -> Bool {
    // The type of the declaration must have been realized.
    guard case .method(let type) = declTypes[id]! else { unreachable() }
    let outputType = type.output.skolemized

    // Type check the generic constraints.
    var success = environment(ofGenericDecl: id) != nil

    // Type check the parameters.
    var parameterNames: Set<String> = []
    for parameter in program.ast[id].parameters {
      success = check(parameter: parameter, siblingNames: &parameterNames) && success
    }

    for impl in program.ast[id].impls {
      // Set the type of the implicit receiver declaration.
      declTypes[program.ast[impl].receiver] = .parameter(ParameterType(
        convention: program.ast[impl].introducer.value.convention,
        bareType: type.receiver))
      declRequests[program.ast[impl].receiver] = .success

      // Type check method's implementations, if any.
      switch program.ast[impl].body {
      case .expr(let expr):
        let expectedType = program.ast[impl].introducer.value == .inout
          ? Type.void
          : outputType
        let inferredType = infer(expr: expr, expectedType: expectedType, inScope: impl)
        success = (inferredType != nil) && success

      case .block(let stmt):
        success = check(brace: stmt) && success

      case nil:
        // Requirements can be without a body.
        if program.isRequirement(id) { continue }

        // Declaration requires a body.
        diagnostics.insert(.diagnose(declarationRequiresBodyAt: program.ast[id].introducerRange))
        success = false
      }
    }

    return success
  }

  private mutating func check(methodImpl: MethodImplDecl) -> Bool {
    fatalError("not implemented")
  }

  private mutating func check(namespace: NamespaceDecl) -> Bool {
    fatalError("not implemented")
  }

  /// Inserts in `siblingNames` the name of the parameter declaration identified by `id` and
  /// returns whether that declaration type checks.
  private mutating func check(
    parameter id: NodeID<ParameterDecl>,
    siblingNames: inout Set<String>
  ) -> Bool {
    // Check for duplicate parameter names.
    if !siblingNames.insert(program.ast[id].name).inserted {
      diagnostics.insert(.diganose(
        duplicateParameterNamed: program.ast[id].name, at: program.ast.ranges[id]))
      declRequests[id] = .failure
      return false
    }

    // Type check the default value, if any.
    if let defaultValue = program.ast[id].defaultValue {
      let parameterType = ParameterType(declTypes[id]!)!
      let defaultValueType = Type.variable(TypeVariable(node: defaultValue.base))

      let constraints = [
        ParameterConstraint(
          defaultValueType,
          .parameter(parameterType),
          because: ConstraintCause(.argument, at: program.ast.ranges[id]))
      ]

      let solution = infer(
        expr: defaultValue,
        inferredType: defaultValueType,
        expectedType: parameterType.bareType,
        inScope: program.declToScope[id]!,
        constraints: constraints)

      if !solution.diagnostics.isEmpty {
        declRequests[id] = .failure
        return false
      }
    }

    declRequests[id] = .success
    return true
  }

  private mutating func check(operator id: NodeID<OperatorDecl>) -> Bool {
    let source = NodeID<TopLevelDeclSet>(program.declToScope[id]!)!

    // Look for duplicate operator declaration.
    for decl in program.ast[source].decls where decl.kind == OperatorDecl.self {
      let oper = NodeID<OperatorDecl>(rawValue: decl.rawValue)
      if oper != id,
         program.ast[oper].notation.value == program.ast[id].notation.value,
         program.ast[oper].name.value == program.ast[id].name.value
      {
        diagnostics.insert(.diagnose(
          duplicateOperatorNamed: program.ast[id].name.value,
          at: program.ast.ranges[id]))
        return false
      }
    }

    return true
  }

  private mutating func check(productType id: NodeID<ProductTypeDecl>) -> Bool {
    _check(decl: id, { (this, id) in this._check(productType: id) })
  }

  private mutating func _check(productType id: NodeID<ProductTypeDecl>) -> Bool {
    // Type check the memberwise initializer.
    var success = check(initializer: program.ast[id].memberwiseInit)

    // Type check the generic constraints.
    success = (environment(ofGenericDecl: id) != nil) && success

    // Type check the type's direct members.
    for j in program.ast[id].members {
      success = check(decl: j) && success
    }

    // Type check conformances.
    let container = program.scopeToParent[id]!
    let traits: Set<TraitType>
    if let ts = realize(conformances: program.ast[id].conformances, inScope: container) {
      traits = ts
    } else {
      traits = []
      success = false
    }

    for trait in traits {
      success = check(conformanceOfProductDecl: id, to: trait) && success
    }

    // Type check extending declarations.
    let type = declTypes[id]!
    for j in extendingDecls(of: type, exposedTo: container) {
      success = check(decl: j) && success
    }

    // TODO: Check the conformances

    return success
  }

  private mutating func check(subscript id: NodeID<SubscriptDecl>) -> Bool {
    _check(decl: id, { (this, id) in this._check(subscript: id) })
  }

  private mutating func _check(subscript id: NodeID<SubscriptDecl>) -> Bool {
    // The type of the declaration must have been realized.
    guard case .subscript(let declType) = declTypes[id]! else { unreachable() }
    let outputType = declType.output.skolemized

    // Type check the generic constraints.
    var success = environment(ofGenericDecl: id) != nil

    // Type check the parameters, if any.
    if let parameters = program.ast[id].parameters {
      var parameterNames: Set<String> = []
      for parameter in parameters {
        success = check(parameter: parameter, siblingNames: &parameterNames) && success
      }
    }

    // Type checks the subscript's implementations.
    for impl in program.ast[id].impls {
      // Set the type of the implicit receiver declaration if necessary.
      if program.isNonStaticMember(id) {
        guard case .remote(let receiverType) = declType.captures.first!.type else { unreachable() }
        let receiverDecl = program.ast[impl].receiver!

        declTypes[receiverDecl] = .parameter(ParameterType(
          convention: program.ast[impl].introducer.value.convention,
          bareType: receiverType.base))
        declRequests[receiverDecl] = .success
      }

      // Type checks the body of the implementation.
      switch program.ast[impl].body {
      case .expr(let expr):
        success = (infer(expr: expr, expectedType: outputType, inScope: impl) != nil) && success

      case .block(let stmt):
        success = check(brace: stmt) && success

      case nil:
        // Requirements can be without a body.
        if program.isRequirement(id) { continue }

        // Declaration requires a body.
        diagnostics.insert(.diagnose(declarationRequiresBodyAt: program.ast[id].introducer.range))
        success = false
      }
    }

    return success
  }

  private mutating func check(subscriptImpl: SubscriptImplDecl) -> Bool {
    fatalError("not implemented")
  }

  private mutating func check(trait id: NodeID<TraitDecl>) -> Bool {
    _check(decl: id, { (this, id) in this._check(trait: id) })
  }

  private mutating func _check(trait id: NodeID<TraitDecl>) -> Bool {
    // Type check the generic constraints.
    var success = environment(ofTraitDecl: id) != nil

    // Type check the type's direct members.
    for j in program.ast[id].members {
      success = check(decl: j) && success
    }

    // Type check extending declarations.
    let type = declTypes[id]!
    for j in extendingDecls(of: type, exposedTo: program.declToScope[id]!) {
      success = check(decl: j) && success
    }

    // TODO: Check the conformances

    return success
  }

  private mutating func check(typeAlias id: NodeID<TypeAliasDecl>) -> Bool {
    _check(decl: id, { (this, id) in this._check(typeAlias: id) })
  }

  private mutating func _check(typeAlias id: NodeID<TypeAliasDecl>) -> Bool {
    // Realize the subject.
    let subject: Type
    switch program.ast[id].body {
    case .typeExpr(let j):
      if let s = realize(j, inScope: AnyScopeID(id)) {
        subject = s
      } else {
        return false
      }

    case.union:
      fatalError("not implemented")
    }

    // Type-check the generic clause.
    var success = environment(ofGenericDecl: id) != nil

    // Type check extending declarations.
    for j in extendingDecls(of: subject, exposedTo: program.declToScope[id]!) {
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
        diagnostics.insert(.diagnose(circularDependencyAt: program.ast.ranges[id]))
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

  /// Type checks the conformance of the product type declared by `decl` to the trait `trait` and
  /// returns whether that succeeded.
  private mutating func check(
    conformanceOfProductDecl decl: NodeID<ProductTypeDecl>,
    to trait: TraitType
  ) -> Bool {
    let conformingType = realizeSelfTypeExpr(inScope: decl)!
    let selfType = Type.genericTypeParam(GenericTypeParamType(decl: trait.decl, ast: program.ast))
    var success = true

    // Get the set of generic parameters defined by `trait`.
    for j in program.ast[trait.decl].members {
      switch j.kind {
      case AssociatedTypeDecl.self:
        // TODO: Implement me.
        continue

      case AssociatedValueDecl.self:
        fatalError("not implemented")

      case FunctionDecl.self:
        // Make sure the requirement is well-typed.
        let requirement = NodeID<FunctionDecl>(rawValue: j.rawValue)
        var requirementType = canonicalize(type: realize(functionDecl: requirement))

        // Substitute `Self` by the conforming type in the requirement type.
        requirementType = requirementType.transform({ type in
          switch type {
          case selfType:
            // `type` is `Self`.
            return .stepOver(conformingType)

          case .associatedType(let t):
            // We only care about associated types rooted at `Self`. Others can be assumed to be
            // rooted at some generic type parameter declared by the requirement.
            let components = t.components
            if components.last != selfType { return .stepOver(type) }

            let scope = AnyScopeID(decl)
            let replacement = components.dropLast(1).reversed()
              .reduce(into: conformingType, { (r, c) in
                if r.isError { return }

                switch c {
                case .associatedType(let c):
                  r = lookupType(named: c.name.value, memberOf: r, inScope: scope)
                    ?? .error(ErrorType())

                case .conformanceLens:
                  fatalError("not implemented")

                default:
                  unreachable()
                }
              })
            return .stepOver(replacement)

          default:
            return .stepInto(type)
          }
        })

        if requirementType.isError { continue }

        // Search for candidate implementations.
        let stem = program.ast[requirement].identifier!.value
        var candidates = lookup(stem, memberOf: conformingType, inScope: AnyScopeID(decl))
        candidates.remove(AnyDeclID(requirement))

        // Filter out the candidates with incompatible types.
        candidates = candidates.filter({ (candidate) -> Bool in
          let candidateType = realize(decl: candidate)
          return canonicalize(type: candidateType) == requirementType
        })

        // TODO: Filter out the candidates with incompatible constraints.
        //
        // trait A {}
        // type Foo<T> {}
        // extension Foo where T: U { fun foo() }
        // conformance Foo: A {} // <- should not consider `foo` in the extension

        // If there are several candidates, we have an ambiguous conformance.
        if candidates.count > 1 {
          fatalError("not implemented")
        }

        // If there's no candidate and the requirement doesn't have a default implementation, the
        // conformance is not satisfied.
        if candidates.isEmpty && (program.ast[requirement].body == nil) {
          diagnostics.insert(
            .diagnose(
              type: conformingType, doesNotConformTo: trait, at: program.ast[decl].identifier.range,
              because: [
                .diagnose(
                  traitRequiresMethod: Name(of: requirement, in: program.ast)!,
                  withType: declTypes[requirement]!)
              ]))
          success = false
        }

      case SubscriptDecl.self:
        fatalError("not implemented")

      default:
        unreachable()
      }
    }

    return success
  }

  /// Type checks the specified statement and returns whether that succeeded.
  private mutating func check<T: StmtID, S: ScopeID>(
    stmt id: T,
    inScope lexicalContext: S
  ) -> Bool {
    switch id.kind {
    case BraceStmt.self:
      return check(brace: NodeID(rawValue: id.rawValue))

    case ExprStmt.self:
      let stmt = program.ast[NodeID<ExprStmt>(rawValue: id.rawValue)]
      if let type = infer(expr: stmt.expr, inScope: lexicalContext) {
        // Issue a warning if the type of the expression isn't void.
        if type != .void {
          diagnostics.insert(.diagnose(unusedResultOfType: type, at: program.ast.ranges[stmt.expr]))
        }
        return true
      } else {
        // Type inference/checking failed.
        return false
      }

    case DeclStmt.self:
      return check(decl: program.ast[NodeID<DeclStmt>(rawValue: id.rawValue)].decl)

    case DiscardStmt.self:
      let stmt = program.ast[NodeID<DiscardStmt>(rawValue: id.rawValue)]
      return infer(expr: stmt.expr, inScope: lexicalContext) != nil

    case ReturnStmt.self:
      return check(return: NodeID(rawValue: id.rawValue), inScope: lexicalContext)

    case YieldStmt.self:
      return check(yield: NodeID(rawValue: id.rawValue), inScope: lexicalContext)

    default:
      unreachable("unexpected statement")
    }
  }

  /// - Note: Method is internal because it may be called during constraint generation.
  mutating func check(brace id: NodeID<BraceStmt>) -> Bool {
    var success = true
    for stmt in program.ast[id].stmts {
      success = check(stmt: stmt, inScope: id) && success
    }
    return success
  }

  private mutating func check<S: ScopeID>(
    return id: NodeID<ReturnStmt>,
    inScope lexicalContext: S
  ) -> Bool {
    // Retreive the expected output type.
    let expectedType = expectedOutputType(in: lexicalContext)!

    if let returnValue = program.ast[id].value {
      // The type of the return value must be subtype of the expected return type.
      let inferredReturnType = Type.variable(TypeVariable(node: returnValue.base))
      let c = equalityOrSubtypingConstraint(
        inferredReturnType,
        expectedType,
        because: ConstraintCause(.return, at: program.ast.ranges[returnValue]))
      let solution = infer(
        expr: returnValue,
        inferredType: inferredReturnType,
        expectedType: expectedType,
        inScope: AnyScopeID(lexicalContext),
        constraints: [c])
      return solution.diagnostics.isEmpty
    } else if expectedType != .void {
      diagnostics.insert(.diagnose(missingReturnValueAt: program.ast.ranges[id]))
      return false
    } else {
      return true
    }
  }

  private mutating func check<S: ScopeID>(
    yield id: NodeID<YieldStmt>,
    inScope lexicalContext: S
  ) -> Bool {
    // Retreive the expected output type.
    let expectedType = expectedOutputType(in: lexicalContext)!

    // The type of the return value must be subtype of the expected return type.
    let inferredReturnType = Type.variable(TypeVariable(node: program.ast[id].value.base))
    let c = equalityOrSubtypingConstraint(
      inferredReturnType,
      expectedType,
      because: ConstraintCause(.yield, at: program.ast.ranges[program.ast[id].value]))
    let solution = infer(
      expr: program.ast[id].value,
      inferredType: inferredReturnType,
      expectedType: expectedType,
      inScope: AnyScopeID(lexicalContext),
      constraints: [c])
    return solution.diagnostics.isEmpty
  }

  /// Returns the expected output type in `lexicalContext`, or `nil` if `lexicalContext` is not
  /// nested in a function or subscript declaration.
  private func expectedOutputType<S: ScopeID>(in lexicalContext: S) -> Type? {
    for parent in program.scopes(from: lexicalContext) {
      switch parent.kind {
      case MethodImplDecl.self:
        // `lexicalContext` is nested in a method implementation.
        let decl = NodeID<MethodImplDecl>(rawValue: parent.rawValue)
        if program.ast[decl].introducer.value == .inout {
          return .void
        } else {
          let methodDecl = NodeID<FunctionDecl>(rawValue: program.scopeToParent[decl]!.rawValue)
          guard case .method(let methodType) = declTypes[methodDecl] else { unreachable() }
          return methodType.output.skolemized
        }

      case FunctionDecl.self:
        // `lexicalContext` is nested in a function.
        let decl = NodeID<FunctionDecl>(rawValue: parent.rawValue)
        guard case .lambda(let funType) = declTypes[decl] else { unreachable() }
        return funType.output.skolemized

      case SubscriptDecl.self:
        // `lexicalContext` is nested in a subscript implementation.
        let decl = NodeID<SubscriptDecl>(rawValue: parent.rawValue)
        guard case .subscript(let subscriptType) = declTypes[decl] else { unreachable() }
        return subscriptType.output.skolemized

      default:
        continue
      }
    }

    return nil
  }

  /// Returns the generic environment defined by `id`, or `nil` if it is ill-typed.
  ///
  /// - Requires: `i.kind <= .genericScope`
  private mutating func environment<T: NodeIDProtocol>(of id: T) -> GenericEnvironment? {
    switch id.kind {
    case FunctionDecl.self:
      return environment(ofGenericDecl: NodeID<FunctionDecl>(rawValue: id.rawValue))
    case ProductTypeDecl.self:
      return environment(ofGenericDecl: NodeID<ProductTypeDecl>(rawValue: id.rawValue))
    case SubscriptDecl.self:
      return environment(ofGenericDecl: NodeID<SubscriptDecl>(rawValue: id.rawValue))
    case TypeAliasDecl.self:
      return environment(ofGenericDecl: NodeID<TypeAliasDecl>(rawValue: id.rawValue))
    case TraitDecl.self:
      return environment(ofTraitDecl: NodeID(rawValue: id.rawValue))
    default:
      unreachable("unexpected scope")
    }
  }

  /// Returns the generic environment defined by `id`, or `nil` if it is ill-typed.
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
    guard let clause = program.ast[id].genericClause?.value else {
      let e = GenericEnvironment(decl: id, constraints: [], into: &self)
      environments[id] = .done(e)
      return e
    }

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
      let list = program.ast[j].conformances
      guard let traits = realize(
        conformances: list,
        inScope: program.scopeToParent[AnyScopeID(id)!]!)
      else { return nil }

      if !traits.isEmpty {
        let cause = ConstraintCause(.annotation, at: program.ast.ranges[list[0]])
        constraints.append(ConformanceConstraint(lhs, traits: traits, because: cause))
      }
    }

    // Evaluate the constraint expressions of the associated type's where clause.
    if let whereClause = clause.whereClause?.value {
      for expr in whereClause.constraints {
        if let constraint = eval(constraintExpr: expr, inScope: AnyScopeID(id)!) {
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

  /// Returns the generic environment defined by `i`, or `nil` if it is ill-typed.
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
    if let whereClause = program.ast[id].whereClause?.value {
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

  /// Returns the generic environment defined by `i`, or `nil` if it is ill-typed.
  private mutating func environment(
    ofTraitDecl id: NodeID<TraitDecl>
  ) -> GenericEnvironment? {
    switch environments[id] {
    case .done(let e):
      return e
    case .inProgress:
      fatalError("circular dependency")
    case nil:
      environments[id] = .inProgress
    }

    var success = true
    var constraints: [Constraint] = []

    // Collect and type check the constraints defined on associated types and values.
    for member in program.ast[id].members {
      switch member.kind {
      case AssociatedTypeDecl.self:
        success = associatedConstraints(
          ofType: NodeID(rawValue: member.rawValue),
          ofTrait: id,
          into: &constraints) && success

      case AssociatedValueDecl.self:
        success = associatedConstraints(
          ofValue: NodeID(rawValue: member.rawValue),
          ofTrait: id,
          into: &constraints) && success

      default:
        continue
      }
    }

    // Bail out if we found ill-form constraints.
    if !success {
      environments[id] = .done(nil)
      return nil
    }

    // Synthesize `Self: T`.
    let selfType = GenericTypeParamType(decl: id, ast: program.ast)
    guard case .trait(let trait) = declTypes[id]! else { unreachable() }
    constraints.append(
      ConformanceConstraint(
        .genericTypeParam(selfType),
        traits: [trait],
        because: ConstraintCause(.structural, at: program.ast[id].identifier.range)))

    let e = GenericEnvironment(decl: id, constraints: constraints, into: &self)
    environments[id] = .done(e)
    return e
  }

  // Evaluates the constraints declared in `associatedType`, stores them in `constraints` and
  // returns whether they are all well-typed.
  private mutating func associatedConstraints(
    ofType associatedType: NodeID<AssociatedTypeDecl>,
    ofTrait trait: NodeID<TraitDecl>,
    into constraints: inout [Constraint]
  ) -> Bool {
    // Realize the LHS of the constraint.
    guard let lhs = realize(decl: associatedType).proper else { return false }

    // Synthesize the sugared conformance constraint, if any.
    let list = program.ast[associatedType].conformances
    guard let traits = realize(
      conformances: list,
      inScope: AnyScopeID(trait))
    else { return false }

    if !traits.isEmpty {
      let cause = ConstraintCause(.annotation, at: program.ast.ranges[list[0]])
      constraints.append(ConformanceConstraint(lhs, traits: traits, because: cause))
    }

    // Evaluate the constraint expressions of the associated type's where clause.
    var success = true
    if let whereClause = program.ast[associatedType].whereClause?.value {
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
  // returns whether they are all well-typed.
  private mutating func associatedConstraints(
    ofValue associatedValue: NodeID<AssociatedValueDecl>,
    ofTrait trait: NodeID<TraitDecl>,
    into constraints: inout [Constraint]
  ) -> Bool {
    // Realize the LHS of the constraint.
    guard !realize(decl: associatedValue).isError else { return false }

    // Evaluate the constraint expressions of the associated value's where clause.
    var success = true
    if let whereClause = program.ast[associatedValue].whereClause?.value {
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
        diagnostics.insert(.diagnose(invalidEqualityConstraintBetween: a, and: b, at: expr.range))
        return nil
      }

      return EqualityConstraint(a, b, because: ConstraintCause(.structural, at: expr.range))

    case .conformance(let l, let traits):
      guard let a = realize(name: l, inScope: scope) else { return nil }
      if !a.isTypeParam {
        diagnostics.insert(.diagnose(invalidConformanceConstraintTo: a, at: expr.range))
        return nil
      }

      var b: Set<TraitType> = []
      for i in traits {
        guard let type = realize(name: i, inScope: scope) else { return nil }
        if case .trait(let trait) = type {
          b.insert(trait)
        } else {
          diagnostics.insert(.diagnose(conformanceToNonTraitType: a, at: expr.range))
          return nil
        }
      }

      return ConformanceConstraint(
        a,
        traits: b,
        because: ConstraintCause(.structural, at: expr.range))

    case .value(let e):
      // TODO: Symbolic execution
      return PredicateConstraint(e, because: ConstraintCause(.structural, at: expr.range))
    }
  }

  // MARK: Type inference

  /// Infers and returns the type of `expr`, or `nil` if `expr` is ill-typed.
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
    constraints: [Constraint]
  ) -> Solution {
    // Generate constraints.
    var generator = ConstraintGenerator(
      scope: scope,
      expr: expr,
      inferredType: inferredType,
      expectedType: expectedType)
    let constraintGeneration = generator.apply(using: &self)

    // Solve the constraints.
    var solver = ConstraintSolver(
      scope: scope,
      fresh: constraints + constraintGeneration.constraints,
      initialDiagnostics: constraintGeneration.diagnostics)
    let solution = solver.apply(using: &self)

    // Apply the solution.
    for (id, type) in constraintGeneration.inferredTypes.storage {
      exprTypes[id] = solution.reify(type, withVariables: .keep)
    }
    for (name, ref) in solution.bindingAssumptions {
      referredDecls[name] = ref
    }

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
  ) -> (type: Type, constraints: [Constraint], decls: [NodeID<VarDecl>])? {
    var constraints: [Constraint] = []
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
    constraints: inout [Constraint],
    decls: inout [NodeID<VarDecl>]
  ) -> Type? {
    switch pattern.kind {
    case BindingPattern.self:
      // A binding pattern introduces additional type information when it has a type annotation. In
      // that case, the type denoted by the annotation is used to infer the type of the sub-pattern
      // and constrained to be a subtype of the expected type, if any.
      let lhs = program.ast[NodeID<BindingPattern>(rawValue: pattern.rawValue)]
      var subpatternType = expectedType
      if let annotation = lhs.annotation {
        if let type = realize(annotation, inScope: scope) {
          if let r = expectedType {
            constraints.append(
              SubtypingConstraint(
                type,
                r,
                because: ConstraintCause(.annotation, at: program.ast.ranges[pattern])))
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

    case _ where pattern.kind.value is Expr.Type:
      fatalError("not implemented")

    case NamePattern.self:
      let lhs = program.ast[NodeID<NamePattern>(rawValue: pattern.rawValue)]
      let type = expectedType ?? .variable(TypeVariable(node: AnyNodeID(lhs.decl)))
      decls.append(lhs.decl)
      declTypes[lhs.decl] = type
      declRequests[lhs.decl] = .typeRealizationCompleted
      return type

    case TuplePattern.self:
      let lhs = program.ast[NodeID<TuplePattern>(rawValue: pattern.rawValue)]
      switch expectedType {
      case .tuple(let rhs):
        // The pattern and the expected have a tuple shape.
        if rhs.elements.count == lhs.elements.count {
          var lLabels: [String?] = []
          var rLabels: [String?] = []

          // Visit the elements pairwise.
          for (a, b) in zip(lhs.elements, rhs.elements) {
            if _infer(
              pattern: a.pattern,
              expectedType: b.type,
              inScope: scope,
              constraints: &constraints,
              decls: &decls) == nil
            { return nil }

            lLabels.append(a.label?.value)
            rLabels.append(b.label)
          }

          // Check that labels match.
          if lLabels == rLabels {
            return expectedType
          } else {
            diagnostics.insert(.diagnose(
              labels: lLabels,
              incompatibleWith: rLabels,
              at: program.ast.ranges[pattern]))
            return nil
          }
        } else {
          // Invalid destructuring.
          diagnostics.insert(.diagnose(
            invalidDestructuringOfType: expectedType!,
            at: program.ast.ranges[pattern]))
          return nil
        }

      case .some:
        // The pattern has a tuple shape, the expected type hasn't.
        diagnostics.insert(.diagnose(
          invalidDestructuringOfType: expectedType!,
          at: program.ast.ranges[pattern]))
        return nil

      case nil:
        // Infer the shape of the expected type.
        var elements: [TupleType.Element] = []
        for element in lhs.elements {
          guard let type = _infer(
            pattern: element.pattern,
            expectedType: nil,
            inScope: scope,
            constraints: &constraints,
            decls: &decls)
          else { return nil }
          elements.append(TupleType.Element(label: element.label?.value, type: type))
        }
        return .tuple(TupleType(elements))
      }

    case WildcardPattern.self:
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

  /// Returns the well-typed declarations to which the specified name may refer, along with their
  /// overarching uncontextualized types. Ill-typed declarations are ignored.
  mutating func resolve(
    _ name: Name,
    introducedInDeclSpaceOf lookupContext: AnyScopeID? = nil,
    inScope origin: AnyScopeID
  ) -> [(decl: AnyDeclID, type: Type)] {
    // Search for the referred declaration.
    var matches: TypeChecker.DeclSet
    if let ctx = lookupContext {
      matches = lookup(name.stem, introducedInDeclSpaceOf: ctx, inScope: origin)
    } else {
      matches = lookup(unqualified: name.stem, inScope: origin)
      if !matches.isEmpty && matches.isSubset(of: bindingsUnderChecking) {
        matches = lookup(unqualified: name.stem, inScope: program.scopeToParent[origin]!)
      }
    }

    // Bail out if there are no matches.
    if matches.isEmpty { return [] }

    // TODO: Filter by labels and operator notation

    // If the looked up name has a method introducer, it must refer to a method implementation.
    if let introducer = name.introducer {
      matches = Set(matches.compactMap({ (match) -> AnyDeclID? in
        guard let decl = NodeID<MethodDecl>(match) else { return nil }

        // TODO: Synthesize missing method implementations
        if let impl = program.ast[decl].impls.first(where: { (i) in
          program.ast[i].introducer.value == introducer
        }) {
          return AnyDeclID(impl)
        } else {
          return nil
        }
      }))
    }

    // Returns the matches along with their contextual type and associated constraints.
    return matches.compactMap({ (match) -> (AnyDeclID, Type)? in
      // Realize the type of the declaration.
      var matchType = realize(decl: match)
      if matchType.isError { return nil }

      // Erase parameter conventions.
      if case .parameter(let t) = matchType {
        matchType = t.bareType
      }

      return (match, matchType)
    })
  }

  /// Returns the declarations that expose `name` without qualification in `scope`.
  mutating func lookup(unqualified name: String, inScope scope: AnyScopeID) -> DeclSet {
    let origin = scope

    var matches = DeclSet()
    var root: NodeID<ModuleDecl>? = nil
    for scope in program.scopes(from: scope) {
      switch scope.kind {
      case ModuleDecl.self:
        // We reached the module scope.
        root = NodeID<ModuleDecl>(rawValue: scope.rawValue)

      case TopLevelDeclSet.self:
        // Skip file scopes so that we don't search the same file twice.
        continue

      default:
        break
      }

      // Search for the name in the current scope.
      let newMatches = lookup(name, introducedInDeclSpaceOf: scope, inScope: origin)

      // We can assume the matches are either empty or all overloadable.
      matches.formUnion(newMatches)

      // We're done if we found at least one non-overloadable match.
      if newMatches.contains(where: { (i) in !(program.ast[i] is FunctionDecl) }) {
        return matches
      }
    }

    // We're done if we found at least one match.
    if !matches.isEmpty { return matches }

    // Check if the name refers to the module containing `scope`.
    if program.ast[root]?.name == name {
      return [AnyDeclID(root!)]
    }

    // Search for the name in imported modules.
    for module in program.ast.modules where module != root {
      matches.formUnion(names(introducedIn: module)[name, default: []])
    }

    return matches
  }

  /// Returns the declarations that introduce `name` in the declaration space of `lookupContext`.
  mutating func lookup<T: ScopeID>(
    _ name: String,
    introducedInDeclSpaceOf lookupContext: T,
    inScope origin: AnyScopeID
  ) -> DeclSet {
    switch lookupContext.kind {
    case ProductTypeDecl.self:
      let type = Type.product(ProductType(
        decl: NodeID(rawValue: lookupContext.rawValue),
        ast: program.ast))
      return lookup(name, memberOf: type, inScope: origin)

    case TraitDecl.self:
      let type = Type.trait(TraitType(
        decl: NodeID(rawValue: lookupContext.rawValue),
        ast: program.ast))
      return lookup(name, memberOf: type, inScope: origin)

    case TypeAliasDecl.self:
      let type = Type.typeAlias(TypeAliasType(
        decl: NodeID(rawValue: lookupContext.rawValue),
        ast: program.ast))
      return lookup(name, memberOf: type, inScope: origin)

    default:
      return names(introducedIn: lookupContext)[name, default: []]
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
    case .boundGeneric(let t):
      matches = lookup(name, memberOf: t.base, inScope: scope)
      return matches

    case .product(let t):
      matches = names(introducedIn: t.decl)[name, default: []]
      if name == "init" {
        matches.insert(AnyDeclID(program.ast[t.decl].memberwiseInit))
      }

    case .trait(let t):
      matches = names(introducedIn: t.decl)[name, default: []]

    case .typeAlias(let t):
      matches = names(introducedIn: t.decl)[name, default: []]

    default:
      matches = DeclSet()
    }

    // We're done if we found at least one non-overloadable match.
    if matches.contains(where: { i in !(program.ast[i] is FunctionDecl) }) {
      return matches
    }

    // Look for members declared in extensions.
    for i in extendingDecls(of: type, exposedTo: scope) {
      matches.formUnion(names(introducedIn: i)[name, default: []])
    }

    // We're done if we found at least one non-overloadable match.
    if matches.contains(where: { i in !(program.ast[i] is FunctionDecl) }) {
      return matches
    }

    // Look for members declared inherited by conformance/refinement.
    guard let traits = conformedTraits(of: type, inScope: scope) else { return matches }
    for trait in traits {
      if type == .trait(trait) { continue }

      // TODO: Read source of conformance to disambiguate associated names
      let newMatches = lookup(name, memberOf: .trait(trait), inScope: scope)
      switch type {
      case .associatedType,
           .genericTypeParam,
           .trait:
        matches.formUnion(newMatches)

      default:
        // Associated type and value declarations are not inherited by conformance.
        matches.formUnion(newMatches.filter({
          $0.kind != AssociatedTypeDecl.self && $0.kind != AssociatedValueDecl.self
        }))
      }
    }

    return matches
  }

  /// Returns the declaration(s) of the specified operator that are visible in `scope`.
  func lookup(
    operator operatorName: Identifier,
    notation: OperatorNotation,
    inScope scope: AnyScopeID
  ) -> [NodeID<OperatorDecl>] {
    let currentModule = program.module(containing: scope)
    if let module = currentModule,
       let oper = lookup(operator: operatorName, notation: notation, in: module)
    {
      return [oper]
    }

    return program.ast.modules.compactMap({ (module) -> NodeID<OperatorDecl>? in
      if module == currentModule { return nil }
      return lookup(operator: operatorName, notation: notation, in: module)
    })
  }

  /// Returns the declaration of the specified operator in `module`, if any.
  func lookup(
    operator operatorName: Identifier,
    notation: OperatorNotation,
    in module: NodeID<ModuleDecl>
  ) -> NodeID<OperatorDecl>? {
    for decl in program.ast.topLevelDecls(module) where decl.kind == OperatorDecl.self {
      let oper = NodeID<OperatorDecl>(rawValue: decl.rawValue)
      if (
        program.ast[oper].notation.value == notation
        && program.ast[oper].name.value == operatorName
      ) {
        return oper
      }
    }
    return nil
  }

  /// Returns the extending declarations of `type` exposed to `scope`.
  ///
  /// - Note: The declarations referred by the returned IDs conform to `TypeExtendingDecl`.
  private mutating func extendingDecls<S: ScopeID>(
    of type: Type,
    exposedTo scope: S
  ) -> [AnyDeclID] {
    var matches: [AnyDeclID] = []
    let canonicalType = canonicalize(type: type)

    /// Inserts into `matches` the conformance and extension declarations contained in `decls`
    /// that extend `canonicalType`.
    func filter<S: Sequence>(this: inout TypeChecker, decls: S, inScope scope: AnyScopeID)
    where S.Element == AnyDeclID
    {
      for i in decls where i.kind == ConformanceDecl.self || i.kind == ExtensionDecl.self {
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
    var root: NodeID<ModuleDecl>? = nil
    for scope in program.scopes(from: scope) {
      switch scope.kind {
      case ModuleDecl.self:
        let module = NodeID<ModuleDecl>(rawValue: scope.rawValue)
        filter(this: &self, decls: program.ast.topLevelDecls(module), inScope: scope)
        root = module

      case TopLevelDeclSet.self:
        continue

      default:
        let decls = program.scopeToDecls[scope, default: []]
        filter(this: &self, decls: decls, inScope: scope)
      }
    }

    // Look for extension declarations in imported modules.
    for module in program.ast.modules where module != root {
      filter(this: &self, decls: program.ast.topLevelDecls(module), inScope: AnyScopeID(module))
    }

    return matches
  }

  /// Returns the names and declarations introduced in `scope`.
  private func names<T: NodeIDProtocol>(introducedIn scope: T) -> LookupTable {
    if let module = NodeID<ModuleDecl>(scope) {
      return program.ast[module].sources.reduce(into: [:], { (table, s) in
        table.merge(names(introducedIn: s), uniquingKeysWith: { (l, _) in l })
      })
    }

    guard let decls = program.scopeToDecls[scope] else { return [:] }
    var table: LookupTable = [:]

    for id in decls {
      switch id.kind {
      case AssociatedValueDecl.self,
           AssociatedTypeDecl.self,
           GenericValueParamDecl.self,
           GenericTypeParamDecl.self,
           NamespaceDecl.self,
           ParameterDecl.self,
           ProductTypeDecl.self,
           TraitDecl.self,
           TypeAliasDecl.self,
           VarDecl.self:
        let name = (program.ast[id] as! SingleEntityDecl).name
        table[name, default: []].insert(id)

      case BindingDecl.self,
           ConformanceDecl.self,
           MethodImplDecl.self,
           OperatorDecl.self,
           SubscriptImplDecl.self:
        // Note: operator declarations are not considered during standard name lookup.
        break

      case FunctionDecl.self:
        let node = program.ast[NodeID<FunctionDecl>(rawValue: id.rawValue)]
        guard let name = node.identifier?.value else { continue }
        table[name, default: []].insert(id)

      case InitializerDecl.self:
        table["init", default: []].insert(id)

      case MethodDecl.self:
        let node = program.ast[NodeID<MethodDecl>(rawValue: id.rawValue)]
        table[node.identifier.value, default: []].insert(id)

      case SubscriptDecl.self:
        let node = program.ast[NodeID<SubscriptDecl>(rawValue: id.rawValue)]
        let name = node.identifier?.value ?? "[]"
        table[name, default: []].insert(id)

      default:
        unreachable("unexpected declaration")
      }
    }

    // Note: Results should be memoized.
    return table
  }

  /// Returns the type named `name` that visible as a member of `type` from `scope`.
  mutating func lookupType(
    named name: String,
    memberOf type: Type,
    inScope scope: AnyScopeID
  ) -> Type? {
    let candidates = lookup(name, memberOf: type, inScope: scope)
    if candidates.count != 1 { return nil }

    let decl = candidates.first!
    if !(decl.kind.value is TypeDecl.Type) { return nil }

    // FIXME: If `type` is a bound generic type, substitute generic type parameters.
    return realize(decl: decl)
  }

  // MARK: Type realization

  /// Realizes and returns the type denoted by `expr` evaluated in `scope`.
  mutating func realize(_ expr: AnyExprID, inScope scope: AnyScopeID) -> Type? {
    switch expr.kind {
    case ConformanceLensTypeExpr.self:
      return realize(conformanceLens: NodeID(rawValue: expr.rawValue), inScope: scope)

    case LambdaTypeExpr.self:
      return realize(lambda: NodeID(rawValue: expr.rawValue), inScope: scope)

    case NameExpr.self:
      return realize(name: NodeID(rawValue: expr.rawValue), inScope: scope)

    case ParameterTypeExpr.self:
      let id = NodeID<ParameterTypeExpr>(rawValue: expr.rawValue)
      diagnostics.insert(.diagnose(
            illegalParameterConvention: program.ast[id].convention.value,
        at: program.ast[id].convention.range))
      return nil

    case TupleTypeExpr.self:
      return realize(tuple: NodeID(rawValue: expr.rawValue), inScope: scope)

    case WildcardTypeExpr.self:
      return .variable(TypeVariable(node: expr.base))

    default:
      unreachable("unexpected expression")
    }
  }

  /// Returns the type `expr`'s the underlying function declaration.
  mutating func realize(underlyingDeclOf expr: NodeID<LambdaExpr>) -> Type? {
    realize(functionDecl: program.ast[expr].decl)
  }

  /// Realizes and returns the type of `Self` in `scope`.
  ///
  /// - Note: This method does not issue diagnostics.
  private mutating func realizeSelfTypeExpr<T: ScopeID>(inScope scope: T) -> Type? {
    for scope in program.scopes(from: scope) {
      switch scope.kind {
      case TraitDecl.self:
        let decl = NodeID<TraitDecl>(rawValue: scope.rawValue)
        return .genericTypeParam(GenericTypeParamType(decl: decl, ast: program.ast))

      case ProductTypeDecl.self:
        // Synthesize unparameterized `Self`.
        let decl = NodeID<ProductTypeDecl>(rawValue: scope.rawValue)
        var type = Type.product(ProductType(decl: decl, ast: program.ast))

        // Synthesize arguments to generic parameters if necessary.
        if let parameters = program.ast[decl].genericClause?.value.parameters {
          let arguments = parameters.map({ (p) -> BoundGenericType.Argument in
            switch p {
            case .type(let p):
              return .type(.genericTypeParam(GenericTypeParamType(decl: p, ast: program.ast)))
            case .value:
              fatalError("not implemented")
            }
          })
          type = .boundGeneric(BoundGenericType(type, arguments: arguments))
        }

        return type

      case ConformanceDecl.self:
        let decl = NodeID<ConformanceDecl>(rawValue: scope.rawValue)
        return realize(program.ast[decl].subject, inScope: scope)

      case ExtensionDecl.self:
        let decl = NodeID<ConformanceDecl>(rawValue: scope.rawValue)
        return realize(program.ast[decl].subject, inScope: scope)

      case TypeAliasDecl.self:
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
    guard let wrapped = realize(program.ast[id].subject, inScope: scope) else { return nil }
    guard let trait = realize(program.ast[id].lens, inScope: scope) else { return nil }

    /// The focus must be a trait.
    guard case .trait(let focus) = trait else {
      diagnostics.insert(.diagnose(notATrait: trait, at: program.ast.ranges[program.ast[id].lens]))
      return nil
    }

    // The base must conform to the focus.
    guard conformedTraits(of: wrapped, inScope: scope)?.contains(focus) ?? false else {
      diagnostics.insert(.diagnose(
        type: wrapped,
        doesNotConformTo: focus,
        at: program.ast.ranges[program.ast[id].lens]))
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
    if let env = program.ast[id].environment?.value {
      guard let ty = realize(env, inScope: scope) else { return nil }
      environment = ty
    } else {
      environment = .any
    }

    // Realize the lambda's parameters.
    var inputs: [CallableTypeParameter] = []
    for parameter in program.ast[id].parameters {
      guard let ty = realize(parameter: parameter.type, inScope: scope) else { return nil }
      inputs.append(CallableTypeParameter(label: parameter.label?.value, type: ty))
    }

    // Realize the lambda's output.
    guard let output = realize(program.ast[id].output, inScope: scope) else { return nil }

    return .lambda(LambdaType(
      receiverEffect: program.ast[id].receiverEffect?.value,
      environment: environment,
      inputs: inputs,
      output: output))
  }

  private mutating func realize(
    name id: NodeID<NameExpr>,
    inScope scope: AnyScopeID
  ) -> Type? {
    let name = program.ast[id].name
    let domain: Type?
    let matches: DeclSet

    switch program.ast[id].domain {
    case .none:
      // Name expression has no domain.
      domain = nil

      // Handle reserved type names.
      switch name.value.stem {
      case "Self":
        if let type = realizeSelfTypeExpr(inScope: scope) {
          return type
        } else {
          diagnostics.insert(.diagnose(invalidReferenceToSelfTypeAt: name.range))
          return nil
        }

      case "Any":
        return .any

      case "Never":
        return .never

      case "Builtin" where isBuiltinModuleVisible:
        return .builtin(.module)

      default:
        break
      }

      // Search for the referred type declaration with an unqualified lookup.
      matches = lookup(unqualified: name.value.stem, inScope: scope)

    case .type(let j):
      // Resolve the domain.
      guard let d = realize(j, inScope: scope) else { return nil }
      domain = d

      // Handle references to built-in types.
      if d == .builtin(.module) {
        if let type = BuiltinType(name.value.stem) {
          return .builtin(type)
        } else {
          diagnostics.insert(.diagnose(noType: name.value, in: domain, at: name.range))
          return nil
        }
      }

      // Search for the referred type declaration with a qualified lookup.
      matches = lookup(name.value.stem, memberOf: d, inScope: scope)

    case .implicit, .expr:
      unreachable("unexpected name domain")
    }

    // Diagnose unresolved names.
    if matches.isEmpty {
      diagnostics.insert(.diagnose(noType: name.value, in: domain, at: name.range))
      return nil
    }

    // Diagnose ambiguous references.
    if matches.count > 1 {
      diagnostics.insert(.diagnose(ambiguousUse: id, in: program.ast))
      return nil
    }

    // Diagnose non-types.
    let match = matches.first!
    if !(match.kind.value is TypeDecl.Type) {
      diagnostics.insert(.diagnose(doesNotEvaluateToType: AnyExprID(id), in: program.ast))
      return nil
    }

    // Realize the referred type.
    let base: Type?

    if match.kind == AssociatedTypeDecl.self {
      let decl = NodeID<AssociatedTypeDecl>(rawValue: match.rawValue)

      switch domain {
      case .associatedType, .conformanceLens, .genericTypeParam:
        base = .associatedType(AssociatedType(decl: decl, domain: domain!, ast: program.ast))

      case nil:
        // Assume that `Self` in `scope` resolves to an implicit generic parameter of a trait
        // declaration, since associated declarations cannot be looked up unqualified outside
        // the scope of a trait and its extensions.
        let domain = realizeSelfTypeExpr(inScope: scope)!
        let type = AssociatedType(
          decl: NodeID(rawValue: match.rawValue), domain: domain, ast: program.ast)
        base = .associatedType(type)

      case .some:
        diagnostics.insert(.diagnose(
          invalidAssociatedTypeNamed: program.ast[decl].name,
          at: name.range))
        return nil
      }
    } else {
      base = realize(decl: match)
    }

    // Bail out if we couldn't realie the name expression.
    if base == nil { return nil }

    // Evaluate the arguments of the referred type, if any.
    if program.ast[id].arguments.isEmpty {
      return base!
    } else {
      var arguments: [BoundGenericType.Argument] = []

      for a in program.ast[id].arguments {
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
    guard let bareType = realize(program.ast[id].bareType, inScope: scope) else { return nil }
    return .parameter(ParameterType(
      convention: program.ast[id].convention.value,
      bareType: bareType))
  }

  private mutating func realize(
    tuple id: NodeID<TupleTypeExpr>,
    inScope scope: AnyScopeID
  ) -> Type? {
    var elements: [TupleType.Element] = []
    elements.reserveCapacity(program.ast[id].elements.count)

    for e in program.ast[id].elements {
      guard let type = realize(e.type, inScope: scope) else { return nil }
      elements.append(TupleType.Element(label: e.label?.value, type: type))
    }

    return .tuple(TupleType(elements))
  }

  /// Realizes and returns the traits of the specified conformance list, or `nil` if at least one
  /// of them is ill-typed.
  private mutating func realize(
    conformances: [NodeID<NameExpr>],
    inScope scope: AnyScopeID
  ) -> Set<TraitType>? {
    // Realize the traits in the conformance list.
    var traits: Set<TraitType> = []
    for expr in conformances {
      guard let rhs = realize(name: expr, inScope: scope) else { return nil }
      if case .trait(let trait) = rhs {
        traits.insert(trait)
      } else {
        diagnostics.insert(.diagnose(conformanceToNonTraitType: rhs, at: program.ast.ranges[expr]))
        return nil
      }
    }

    return traits
  }

  /// Returns the overarching type of the specified declaration.
  mutating func realize<T: DeclID>(decl id: T) -> Type {
    switch id.kind {
    case AssociatedTypeDecl.self:
      return _realize(decl: id, { (this, id) in
        let traitDecl = NodeID<TraitDecl>(rawValue: this.program.declToScope[id]!.rawValue)
        return .associatedType(AssociatedType(
          decl: NodeID(rawValue: id.rawValue),
          domain: .genericTypeParam(GenericTypeParamType(decl: traitDecl, ast: this.program.ast)),
          ast: this.program.ast))
      })

    case AssociatedValueDecl.self:
      return _realize(decl: id, { (this, id) in
        let traitDecl = NodeID<TraitDecl>(rawValue: this.program.declToScope[id]!.rawValue)
        return .associatedValue(AssociatedValueType(
          decl: NodeID(rawValue: id.rawValue),
          domain: .genericTypeParam(GenericTypeParamType(decl: traitDecl, ast: this.program.ast)),
          ast: this.program.ast))
      })

    case GenericTypeParamDecl.self:
      return _realize(decl: id, { (this, id) in
        .genericTypeParam(GenericTypeParamType(decl: id, ast: this.program.ast))
      })

    case GenericValueParamDecl.self:
      return _realize(decl: id, { (this, id) in
        .genericValueParam(GenericValueParamType(decl: id, ast: this.program.ast))
      })

    case BindingDecl.self:
      return realize(bindingDecl: NodeID(rawValue: id.rawValue))

    case ConformanceDecl.self,
         ExtensionDecl.self:
      return _realize(decl: id, { (this, id) in
        let decl = this.program.ast[id] as! TypeExtendingDecl
        return this.realize(decl.subject, inScope: this.program.declToScope[id]!)
      })

    case FunctionDecl.self:
      return realize(functionDecl: NodeID(rawValue: id.rawValue))

    case InitializerDecl.self:
      return realize(initializerDecl: NodeID(rawValue: id.rawValue))

    case MethodDecl.self:
      return realize(methodDecl: NodeID(rawValue: id.rawValue))

    case MethodImplDecl.self:
      return realize(methodDecl: NodeID(rawValue: program.declToScope[id]!.rawValue))

    case ParameterDecl.self:
      return realize(parameterDecl: NodeID(rawValue: id.rawValue))

    case ProductTypeDecl.self:
      return _realize(decl: id, { (this, id) in
        .product(ProductType(decl: NodeID(rawValue: id.rawValue), ast: this.program.ast))
      })

    case SubscriptDecl.self:
      return realize(subscriptDecl: NodeID(rawValue: id.rawValue))

    case TraitDecl.self:
      return _realize(decl: id, { (this, id) in
        .trait(TraitType(decl: NodeID(rawValue: id.rawValue), ast: this.program.ast))
      })

    case TypeAliasDecl.self:
      return _realize(decl: id, { (this, id) in
        .typeAlias(TypeAliasType(decl: NodeID(rawValue: id.rawValue), ast: this.program.ast))
      })

    case VarDecl.self:
      let bindingDecl = program.varToBinding[NodeID(rawValue: id.rawValue)]!
      let bindingType = realize(bindingDecl: bindingDecl)
      return bindingType.isError
        ? bindingType
        : declTypes[id]!

    default:
      unreachable("unexpected declaration")
    }
  }

  private mutating func realize(bindingDecl id: NodeID<BindingDecl>) -> Type {
    _ = check(binding: NodeID(rawValue: id.rawValue))
    return declTypes[id]!
  }

  private mutating func realize(functionDecl id: NodeID<FunctionDecl>) -> Type {
    _realize(decl: id, { (this, id) in this._realize(functionDecl: id) })
  }

  private mutating func _realize(functionDecl id: NodeID<FunctionDecl>) -> Type {
    var success = true

    // Realize the input types.
    var inputs: [CallableTypeParameter] = []
    for i in program.ast[id].parameters {
      declRequests[i] = .typeCheckingStarted

      if let annotation = program.ast[i].annotation {
        if let type = realize(parameter: annotation, inScope: AnyScopeID(id)) {
          // The annotation may not omit generic arguments.
          if type[.hasVariable] {
            diagnostics.insert(.diagnose(
              notEnoughContextToInferArgumentsAt: program.ast.ranges[annotation]))
            success = false
          }

          declTypes[i] = type
          declRequests[i] = .typeRealizationCompleted
          inputs.append(CallableTypeParameter(label: program.ast[i].label?.value, type: type))
        } else {
          declTypes[i] = .error(ErrorType())
          declRequests[i] = .failure
          success = false
        }
      } else {
        // Note: parameter type annotations may be elided if the declaration represents a lambda
        // expression. In that case, the unannotated parameters are associated with a fresh type
        // so inference can proceed. The actual type of the parameter will be reified during type
        // checking, when `checkPending` is called.
        if program.ast[id].isInExprContext {
          let parameterType = Type.variable(TypeVariable(node: AnyNodeID(i)))
          declTypes[i] = parameterType
          declRequests[i] = .typeRealizationCompleted
          inputs.append(CallableTypeParameter(
            label: program.ast[i].label?.value,
            type: parameterType))
        } else {
          unreachable("expected type annotation")
        }
      }
    }

    // Bail out if the parameters could not be realized.
    if !success { return .error(ErrorType()) }

    // Collect captures.
    var explicitCaptureNames: Set<Name> = []
    guard let explicitCaptureTypes = realize(
      explicitCaptures: program.ast[id].explicitCaptures,
      collectingNamesIn: &explicitCaptureNames)
    else { return .error(ErrorType()) }

    let implicitCaptures: [ImplicitCapture] = program.isLocal(id)
      ? realize(implicitCapturesIn: id, ignoring: explicitCaptureNames)
      : []
    self.implicitCaptures[id] = implicitCaptures

    // Realize the function's receiver if necessary.
    let isNonStaticMember = program.isNonStaticMember(id)
    var receiver: Type? = isNonStaticMember
      ? realizeSelfTypeExpr(inScope: program.declToScope[id]!)
      : nil

    // Realize the output type.
    let outputType: Type
    if let o = program.ast[id].output {
      // Use the explicit return annotation.
      guard let type = realize(o, inScope: AnyScopeID(id)) else { return .error(ErrorType()) }
      outputType = type
    } else if program.ast[id].isInExprContext {
      // Infer the return type from the body in expression contexts.
      outputType = .variable(TypeVariable())
    } else {
      // Default to `Void`.
      outputType = .void
    }

    if isNonStaticMember {
      // Create a lambda bound to a receiver.
      let effect: ReceiverEffect?
      if program.ast[id].isInout {
        receiver = .tuple(TupleType([
          TupleType.Element(label: "self", type: .remote(RemoteType(.inout, receiver!)))
        ]))
        effect = .inout
      } else if program.ast[id].isSink  {
        receiver = .tuple(TupleType([
          TupleType.Element(label: "self", type: receiver!)
        ]))
        effect = .sink
      } else {
        receiver = .tuple(TupleType([
          TupleType.Element(label: "self", type: .remote(RemoteType(.let, receiver!)))
        ]))
        effect = nil
      }

      return .lambda(LambdaType(
        receiverEffect: effect,
        environment: receiver!,
        inputs: inputs,
        output: outputType))
    } else {
      // Create a regular lambda.
      let environment = Type.tuple(TupleType(
        explicitCaptureTypes.map({ (t) in TupleType.Element(label: nil, type: t) }) +
        implicitCaptures.map({ (c) in TupleType.Element(label: nil, type: .remote(c.type)) })
      ))

      // TODO: Determine if the lambda is mutating.

      return .lambda(LambdaType(
        environment: environment,
        inputs: inputs,
        output: outputType))
    }
  }

  private mutating func realize(initializerDecl id: NodeID<InitializerDecl>) -> Type {
    _realize(decl: id, { (this, id) in this._realize(initializerDecl: id) })
  }

  private mutating func _realize(initializerDecl id: NodeID<InitializerDecl>) -> Type {
    // Handle memberwise initializers.
    if program.ast[id].introducer.value == .memberwiseInit {
      let productTypeDecl = NodeID<ProductTypeDecl>(program.declToScope[id]!)!
      if let lambda = memberwiseInitType(of: productTypeDecl) {
        return .lambda(lambda)
      } else {
        return .error(ErrorType())
      }
    }

    var success = true

    // Realize the input types.
    var inputs: [CallableTypeParameter] = []
    for i in program.ast[id].parameters {
      declRequests[i] = .typeCheckingStarted

      // Parameters of initializers must have a type annotation.
      guard let annotation = program.ast[i].annotation else {
        unreachable("unexpected type expression")
      }

      if let type = realize(parameter: annotation, inScope: AnyScopeID(id)) {
        // The annotation may not omit generic arguments.
        if type[.hasVariable] {
          diagnostics.insert(.diagnose(notEnoughContextToInferArgumentsAt: program.ast.ranges[annotation]))
          success = false
        }

        declTypes[i] = type
        declRequests[i] = .typeRealizationCompleted
        inputs.append(CallableTypeParameter(label: program.ast[i].label?.value, type: type))
      } else {
        declTypes[i] = .error(ErrorType())
        declRequests[i] = .failure
        success = false
      }
    }

    // Bail out if the parameters could not be realized.
    if !success { return .error(ErrorType()) }

    // Initializers are global functions.
    let receiverType = realizeSelfTypeExpr(inScope: program.declToScope[id]!)
    let receiverParameterType = CallableTypeParameter(
      label: "self",
      type: .parameter(ParameterType(convention: .set, bareType: receiverType!)))
    inputs.insert(receiverParameterType, at: 0)
    return .lambda(LambdaType(environment: .void, inputs: inputs, output: .void))
  }

  private mutating func realize(methodDecl id: NodeID<MethodDecl>) -> Type {
    _realize(decl: id, { (this, id) in this._realize(methodDecl: id) })
  }

  private mutating func _realize(methodDecl id: NodeID<MethodDecl>) -> Type {
    var success = true

    // Realize the input types.
    var inputs: [CallableTypeParameter] = []
    for i in program.ast[id].parameters {
      declRequests[i] = .typeCheckingStarted

      // Parameters of methods must have a type annotation.
      guard let annotation = program.ast[i].annotation else {
        unreachable("unexpected type expression")
      }

      if let type = realize(parameter: annotation, inScope: AnyScopeID(id)) {
        // The annotation may not omit generic arguments.
        if type[.hasVariable] {
          diagnostics.insert(.diagnose(notEnoughContextToInferArgumentsAt: program.ast.ranges[annotation]))
          success = false
        }

        declTypes[i] = type
        declRequests[i] = .typeRealizationCompleted
        inputs.append(CallableTypeParameter(label: program.ast[i].label?.value, type: type))
      } else {
        declTypes[i] = .error(ErrorType())
        declRequests[i] = .failure
        success = false
      }
    }

    // Bail out if the parameters could not be realized.
    if !success { return .error(ErrorType()) }

    // Realize the method's receiver if necessary.
    let receiver = realizeSelfTypeExpr(inScope: program.declToScope[id]!)

    // Realize the output type.
    let outputType: Type
    if let o = program.ast[id].output {
      // Use the explicit return annotation.
      guard let type = realize(o, inScope: AnyScopeID(id)) else { return .error(ErrorType()) }
      outputType = type
    } else {
      // Default to `Void`.
      outputType = .void
    }

    // Create a method bundle.
    let capabilities = Set(program.ast[id].impls.map({ program.ast[$0].introducer.value }))
    if capabilities.contains(.inout) && (outputType != receiver) {
      let range = program.ast[id].output.map({ (output) in
        program.ast.ranges[output]
      }) ?? program.ast[id].introducerRange
      diagnostics.insert(.diagnose(inoutCapableMethodBundleMustReturn: receiver!, at: range))
      return .error(ErrorType())
    }

    return .method(MethodType(
      capabilities: capabilities,
      receiver: receiver!,
      inputs: inputs,
      output: outputType))
  }

  /// Returns the overarching type of the specified parameter declaration.
  ///
  /// - Requires: The containing function or subscript declaration must have been realized.
  private mutating func realize(parameterDecl id : NodeID<ParameterDecl>) -> Type {
    switch declRequests[id] {
    case nil:
      preconditionFailure()

    case .typeRealizationStarted:
      diagnostics.insert(.diagnose(circularDependencyAt: program.ast.ranges[id]))
      return .error(ErrorType())

    case .typeRealizationCompleted, .typeCheckingStarted, .success, .failure:
      return declTypes[id]!
    }
  }

  private mutating func realize(subscriptDecl id: NodeID<SubscriptDecl>) -> Type {
    _realize(decl: id, { (this, id) in this._realize(subscriptDecl: id) })
  }

  private mutating func _realize(subscriptDecl id: NodeID<SubscriptDecl>) -> Type {
    var success = true

    // Realize the input types.
    var inputs: [CallableTypeParameter] = []
    for i in program.ast[id].parameters ?? [] {
      declRequests[i] = .typeCheckingStarted

      // Parameters of subscripts must have a type annotation.
      guard let annotation = program.ast[i].annotation else {
        unreachable("unexpected type expression")
      }

      if let type = realize(parameter: annotation, inScope: AnyScopeID(id)) {
        // The annotation may not omit generic arguments.
        if type[.hasVariable] {
          diagnostics.insert(.diagnose(notEnoughContextToInferArgumentsAt: program.ast.ranges[annotation]))
          success = false
        }

        declTypes[i] = type
        declRequests[i] = .typeRealizationCompleted
        inputs.append(CallableTypeParameter(label: program.ast[i].label?.value, type: type))
      } else {
        declTypes[i] = .error(ErrorType())
        declRequests[i] = .failure
        success = false
      }
    }

    // Bail out if the parameters could not be realized.
    if !success { return .error(ErrorType()) }

    // Collect captures.
    var explicitCaptureNames: Set<Name> = []
    guard let explicitCaptureTypes = realize(
      explicitCaptures: program.ast[id].explicitCaptures,
      collectingNamesIn: &explicitCaptureNames)
    else { return .error(ErrorType()) }

    let implicitCaptures: [ImplicitCapture] = program.isLocal(id)
      ? realize(implicitCapturesIn: id, ignoring: explicitCaptureNames)
      : []
    self.implicitCaptures[id] = implicitCaptures

    // Build the subscript's environment.
    let environment: TupleType
    if program.isNonStaticMember(id) {
      let receiver = realizeSelfTypeExpr(inScope: program.declToScope[id]!)
      environment = TupleType([
        TupleType.Element(label: "self", type: .remote(RemoteType(.yielded, receiver!)))
      ])
    } else {
      environment = TupleType(
        explicitCaptureTypes.map({ (t) in TupleType.Element(label: nil, type: t) }) +
        implicitCaptures.map({ (c) in TupleType.Element(label: nil, type: .remote(c.type)) })
      )
    }

    // Realize the ouput type.
    guard let output = realize(program.ast[id].output, inScope: AnyScopeID(id)) else {
      return .error(ErrorType())
    }

    // Create a subscript type.
    let capabilities = Set(program.ast[id].impls.map({ program.ast[$0].introducer.value }))
    return .subscript(SubscriptType(
      isProperty: program.ast[id].parameters == nil,
      capabilities: capabilities,
      environment: .tuple(environment),
      inputs: inputs,
      output: output))
  }

  /// Realizes the explicit captures in `list`, writing the captured names in `explicitNames`, and
  /// returns their types if they are semantically well-typed. Otherwise, returns `nil`.
  private mutating func realize(
    explicitCaptures list: [NodeID<BindingDecl>],
    collectingNamesIn explictNames: inout Set<Name>
  ) -> [Type]? {
    var explictNames: Set<Name> = []
    var captures: [Type] = []
    var success = true

    // Process explicit captures.
    for i in list {
      // Collect the names of the capture.
      for (_, namePattern) in program.ast.names(in: program.ast[i].pattern) {
        let varDecl = program.ast[namePattern].decl
        if !explictNames.insert(Name(stem: program.ast[varDecl].name)).inserted {
          diagnostics.insert(.diagnose(
            duplicateCaptureNamed: program.ast[varDecl].name,
            at: program.ast.ranges[varDecl]))
          success = false
        }
      }

      // Realize the type of the capture.
      if let type = realize(bindingDecl: i).proper {
        switch program.ast[program.ast[i].pattern].introducer.value {
        case .let:
          captures.append(.remote(RemoteType(.let, type)))
        case .inout:
          captures.append(.remote(RemoteType(.inout, type)))
        case .sinklet, .var:
          captures.append(type)
        }
      } else {
        success = false
      }
    }

    return success ? captures : nil
  }

  /// Realizes the implicit captures found in the body of `decl` and returns their types and
  /// declarations if they are well-typed. Otherwise, returns `nil`.
  private mutating func realize<T: Decl & LexicalScope>(
    implicitCapturesIn decl: NodeID<T>,
    ignoring explictNames: Set<Name>
  ) -> [ImplicitCapture] {
    // Process implicit captures.
    var captures: [ImplicitCapture] = []
    var receiverIndex: Int? = nil

    var collector = CaptureCollector(ast: program.ast)
    for (name, uses) in collector.freeNames(in: decl) {
      // Explicit captures are already accounted for.
      if explictNames.contains(name) { continue }

      // Resolve the name.
      let matches = lookup(unqualified: name.stem, inScope: program.declToScope[decl]!)

      // If there are multiple matches, attempt to filter them using the name's argument labels or
      // operator notation. If that fails, complain about an ambiguous implicit capture.
      let captureDecl: AnyDeclID
      switch matches.count {
      case 0:
        continue
      case 1:
        captureDecl = matches.first!
      default:
        fatalError("not implemented")
      }

      // Global declarations are not captured.
      if program.isGlobal(captureDecl) { continue }

      // References to member declarations implicitly capture of their receiver.
      if program.isMember(captureDecl) {
        // If the function refers to a member declaration, it must be nested in a type scope.
        let innermostTypeScope = program
          .scopes(from: program.scopeToParent[decl]!)
          .first(where: { $0.kind.value is TypeDecl.Type })!

        // Ignore illegal implicit references to foreign receiver.
        if program.isContained(innermostTypeScope, in: program.scopeToParent[captureDecl]!) {
          continue
        }

        if let i = receiverIndex, uses.capability != .let {
          // Update the mutability of the capture.
          captures[i] = ImplicitCapture(
            name: captures[i].name,
            type: RemoteType(.inout, captures[i].type.base),
            decl: captures[i].decl)
        } else {
          // Resolve the implicit reference to `self`.
          let receiverMatches = lookup(unqualified: "self", inScope: program.scopeToParent[decl]!)
          let receiverDecl: AnyDeclID
          switch receiverMatches.count {
          case 0:
            continue
          case 1:
            receiverDecl = matches.first!
          default:
            unreachable()
          }

          // Realize the type of `self`.
          let receiverType = realize(decl: receiverDecl)
          if receiverType.isError { continue }

          // Register the capture of `self`.
          receiverIndex = captures.count
          captures.append(ImplicitCapture(
            name: Name(stem: "self"),
            type: RemoteType(uses.capability, receiverType.skolemized),
            decl: receiverDecl))
        }

        continue
      }

      // Capture-less local functions are not captured.
      if let d = NodeID<FunctionDecl>(captureDecl) {
        guard case .lambda(let lambda) = realize(functionDecl: d) else { continue }
        if lambda.environment == .void { continue }
      }

      // Other local declarations are captured.
      guard let captureType = realize(decl: captureDecl).proper?.skolemized else { continue }
      captures.append(ImplicitCapture(
        name: name,
        type: RemoteType(uses.capability, captureType),
        decl: captureDecl))
    }

    return captures
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
      diagnostics.insert(.diagnose(circularDependencyAt: program.ast.ranges[id]))
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
    for m in program.ast[decl].members {
      guard let member = NodeID<BindingDecl>(m) else { continue }
      if realize(bindingDecl: member).isError { return nil }

      for (_, name) in program.ast.names(in: program.ast[member].pattern) {
        let d = program.ast[name].decl
        inputs.append(CallableTypeParameter(
          label: program.ast[d].name,
          type: .parameter(ParameterType(convention: .sink, bareType: declTypes[d]!))))
      }
    }

    return LambdaType(environment: .void, inputs: inputs, output: .void)
  }

  // MARK: Type role determination

  /// Skolemizes `type`.
  func skolemize(type: Type) -> Type {
    return type.transform({ type in
      switch type {
      case .associatedType,
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
  func open(type: Type) -> (Type, ConstraintSet) {
    var openedParameters: [Type: Type] = [:]

    let transformed = type.transform({ type in
      switch type {
      case .associatedType:
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
  func contextualize<S: ScopeID>(
    type: Type,
    inScope scope: S,
    cause: ConstraintCause
  ) -> (Type, ConstraintSet) {
    var openedParameters: [Type: Type] = [:]

    let transformed = type.transform({ type in
      switch type {
      case .associatedType:
        fatalError("not implemented")

      case .genericTypeParam(let base):
        // Identify the generic environment that introduces the parameter.
        let origin: AnyScopeID
        if base.decl.kind == TraitDecl.self {
          origin = AnyScopeID(base.decl)!
        } else {
          origin = program.declToScope[base.decl]!
        }

        if program.isContained(scope, in: origin) {
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

  /// Resets `self` to an empty state, returning `self`'s old value.
  mutating func release() -> Self {
    var r: Self = .init(program: program)
    swap(&r, &self)
    return r
  }
}
