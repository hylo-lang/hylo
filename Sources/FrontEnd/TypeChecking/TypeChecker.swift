import Core
import OrderedCollections
import Utils

/// Val's type checker.
public struct TypeChecker {

  /// The program being type checked.
  public let program: ScopedProgram

  /// The diagnostics of the type errors.
  private(set) var diagnostics: DiagnosticSet = []

  /// A map from translation unit to its imports.
  private(set) var imports: [TranslationUnit.ID: Set<ModuleDecl.ID>] = [:]

  /// The overarching type of each declaration.
  private(set) var declTypes = DeclProperty<AnyType>()

  /// The type of each expression.
  private(set) var exprTypes = ExprProperty<AnyType>()

  /// A map from function and subscript declarations to their implicit captures.
  private(set) var implicitCaptures = DeclProperty<[ImplicitCapture]>()

  /// A map from module to its synthesized declarations.
  private(set) var synthesizedDecls: [ModuleDecl.ID: [SynthesizedDecl]] = [:]

  /// A map from name expression to its referred declaration.
  private(set) var referredDecls: BindingMap = [:]

  /// A map from sequence expressions to their evaluation order.
  var foldedSequenceExprs: [SequenceExpr.ID: FoldedSequenceExpr] = [:]

  /// The type relations of the program.
  private(set) var relations = TypeRelations()

  /// Indicates whether the built-in symbols are visible.
  var isBuiltinModuleVisible: Bool

  /// The site for which type inference tracing is enabled, if any.
  private let inferenceTracingSite: SourceLine?

  /// Creates a new type checker for the specified program.
  ///
  /// - Note: `program` is stored in the type checker and mutated throughout type checking (e.g.,
  ///   to insert synthesized declarations).
  public init(
    program: ScopedProgram,
    isBuiltinModuleVisible: Bool = false,
    tracingInferenceIn inferenceTracingSite: SourceLine? = nil
  ) {
    self.program = program
    self.isBuiltinModuleVisible = isBuiltinModuleVisible
    self.inferenceTracingSite = inferenceTracingSite
  }

  /// The AST of the program being type checked.
  public var ast: AST { program.ast }

  /// Reports the given diagnostic.
  mutating func report(_ d: Diagnostic) {
    diagnostics.insert(d)
  }

  // MARK: Type system

  /// Returns a copy of `generic` where occurrences of parameters keying `subtitutions` are
  /// replaced by their corresponding value, performing necessary name lookups from `useScope`.
  ///
  /// This method has no effect if `substitutions` is empty.
  private mutating func specialized(
    _ generic: AnyType, applying substitutions: GenericArguments, in useScope: AnyScopeID
  ) -> AnyType {
    return substitutions.isEmpty ? generic : generic.transform(specialize(_:))

    func specialize(_ t: AnyType) -> TypeTransformAction {
      switch t.base {
      case let u as GenericTypeParameterType:
        if let v = substitutions[u.decl] {
          return .stepOver((v as? AnyType) ?? .error)
        } else {
          return .stepOver(t)
        }

      case let u as AssociatedTypeType:
        let d = u.domain.transform(specialize(_:))

        let candidates = lookup(ast[u.decl].baseName, memberOf: d, exposedTo: useScope)
        if let c = candidates.uniqueElement {
          return .stepOver(MetatypeType(realize(decl: c))?.instance ?? .error)
        } else {
          return .stepOver(.error)
        }

      case let u as BoundGenericType:
        let updatedArguments = u.arguments.mapValues { (v) -> any CompileTimeValue in
          if let w = v as? AnyType {
            return specialized(w, applying: substitutions, in: useScope)
          } else {
            return v
          }
        }
        return .stepOver(^BoundGenericType(u.base, arguments: updatedArguments))

      default:
        return .stepInto(t)
      }
    }
  }

  /// If `generic` is an unbound generic type, returns a bound generic type mapping its parameters
  /// to corresponding value in `substitutions` or a fresh variable if no such value exists.
  /// Otherwise, returns `generic` unchanged.
  private mutating func bind(_ generic: AnyType, to substitutions: GenericArguments) -> AnyType {
    let filtered: GenericArguments?

    switch generic.base {
    case let u as ProductType:
      filtered = extractArguments(of: u.decl, from: substitutions)
    case let u as TypeAliasType:
      filtered = extractArguments(of: u.decl, from: substitutions)
    case let u as MetatypeType:
      return ^MetatypeType(of: bind(u.instance, to: substitutions))
    default:
      return generic
    }

    return filtered.map({ ^BoundGenericType(generic, arguments: $0) }) ?? generic
  }

  /// If `d` has generic parameters, returns a table from those parameters to corresponding value
  /// in `substitutions` or a fresh variable if no such value exists. Otherwise, returns `nil`.
  private mutating func extractArguments<T: GenericDecl>(
    of d: T.ID,
    from substitutions: GenericArguments
  ) -> GenericArguments? {
    let e = environment(of: d)
    if e.parameters.isEmpty { return nil }

    return GenericArguments(
      uniqueKeysWithValues: e.parameters.map({ (p) in
        (key: p, value: substitutions[p] ?? ^TypeVariable())
      }))
  }

  /// Returns the set of traits to which `type` conforms in `useScope`, visiting all conformance
  /// and refinement declarations recursively.
  ///
  /// The returned set only contains traits whose expressions could be evaluated. A diagnostic is
  /// reported for all other expressions. `type` is contained in the returned set if it's a trait.
  mutating func conformedTraits(of type: AnyType, in useScope: AnyScopeID) -> Set<TraitType> {
    var result: Set<TraitType> = []

    switch type.base {
    case let t as GenericTypeParameterType:
      // Generic parameters declared at trait scope conform to that trait.
      if let decl = TraitDecl.ID(program.declToScope[t.decl]!) {
        return conformedTraits(of: ^TraitType(decl, ast: ast), in: useScope)
      }

      // Conformances of other generic parameters are stored in generic environments.
      for s in program.scopes(from: useScope) where useScope.kind.value is GenericScope.Type {
        let e = environment(of: s)
        result.formUnion(e.conformedTraits(of: type))
      }

    case let t as ProductType:
      let s = program.declToScope[t.decl]!
      for t in realize(conformances: ast[t.decl].conformances, in: s) {
        result.formUnion(conformedTraits(of: ^t, in: s))
      }

    case let t as TraitType:
      result.insert(t)

      let s = program.declToScope[t.decl]!
      var work = realize(conformances: ast[t.decl].refinements, in: s)
      while let base = work.popFirst() {
        if base == t {
          diagnostics.insert(.error(circularRefinementAt: ast[t.decl].identifier.site))
        } else if result.insert(base).inserted {
          let newTraits = realize(
            conformances: ast[base.decl].refinements, in: program.scopeToParent[base.decl]!)
          work.formUnion(newTraits)
        }
      }

      // Traits can't be refined in extensions; we're done.
      return result

    default:
      break
    }

    // Collect traits declared in conformance declarations.
    for i in extendingDecls(of: type, exposedTo: useScope) where i.kind == ConformanceDecl.self {
      let s = program.declToScope[i]!
      for t in realize(conformances: ast[ConformanceDecl.ID(i)!].conformances, in: s) {
        result.formUnion(conformedTraits(of: ^t, in: s))
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

    /// Type checking was completed.
    ///
    /// The overarching type is availabe in `declTypes`.
    case done

  }

  /// A cache for type checking requests on declarations.
  private var declRequests = DeclProperty<RequestStatus>()

  /// A cache mapping generic declarations to their environment.
  private var environments = DeclProperty<MemoizationState<GenericEnvironment>>()

  /// The bindings whose initializers are being currently visited.
  private var bindingsUnderChecking: DeclSet = []

  /// Sets the inferred type of `d` to `type`.
  ///
  /// - Requires: `d` has been assigned to a type yet.
  mutating func setInferredType(_ type: AnyType, for d: VarDecl.ID) {
    precondition(declTypes[d] == nil)
    declTypes[d] = type
  }

  /// Type checks the specified module, accumulating diagnostics in `self.diagnostics`
  ///
  /// This method is idempotent. After the first call for a module `m`, `self.declTypes[m]` is
  /// assigned to an instance of `ModuleType`. Subsequent calls have no effect on `self`.
  ///
  /// - Requires: `m` is a valid ID in the type checker's AST.
  public mutating func check(module m: ModuleDecl.ID) {
    _check(decl: m) { (this, m) in
      this.ast[m].sources.forEach({ this.check(translationUnit: $0) })
    }
  }

  /// Type checks all declarations in `u`.
  private mutating func check(translationUnit u: TranslationUnit.ID) {
    // The core library is always implicitly imported.
    if let m = ast.coreLibrary { imports[u] = [m] }
    for d in program.scopeToDecls[u]!.lazy.compactMap(ImportDecl.ID.init(_:)) {
      registerImport(d, in: u)
    }
    check(all: ast[u].decls)
  }

  /// Register the import declared by `d` in translation unit `u`.
  private mutating func registerImport(_ d: ImportDecl.ID, in u: TranslationUnit.ID) {
    guard let m = ModuleType(realize(importDecl: d))?.decl else { return }
    if program.module(containing: u) != m {
      imports[u, default: []].insert(m)
    } else {
      diagnostics.insert(.warning(needlessImport: d, in: ast))
    }
  }

  /// Type checks all declarations in `batch`.
  private mutating func check<S: Sequence<AnyDeclID>>(all batch: S) {
    batch.forEach({ check(decl: $0) })
  }

  /// Type checks `d`.
  private mutating func check<T: DeclID>(decl d: T) {
    switch d.kind {
    case AssociatedTypeDecl.self:
      break
    case AssociatedValueDecl.self:
      break
    case BindingDecl.self:
      check(binding: NodeID(d)!)
    case ConformanceDecl.self:
      check(conformance: NodeID(d)!)
    case ExtensionDecl.self:
      check(extension: NodeID(d)!)
    case FunctionDecl.self:
      check(function: NodeID(d)!)
    case GenericParameterDecl.self:
      check(genericParameter: NodeID(d)!)
    case ImportDecl.self:
      check(importDecl: NodeID(d)!)
    case InitializerDecl.self:
      check(initializer: NodeID(d)!)
    case MethodDecl.self:
      check(method: NodeID(d)!)
    case MethodImpl.self:
      check(method: NodeID(program.declToScope[d]!)!)
    case NamespaceDecl.self:
      check(namespace: NodeID(d)!)
    case OperatorDecl.self:
      check(operator: NodeID(d)!)
    case ProductTypeDecl.self:
      check(productType: NodeID(d)!)
    case SubscriptDecl.self:
      check(subscript: NodeID(d)!)
    case TraitDecl.self:
      check(trait: NodeID(d)!)
    case TypeAliasDecl.self:
      check(typeAlias: NodeID(d)!)
    default:
      unexpected(d, in: ast)
    }
  }

  private mutating func check(associatedType d: AssociatedTypeDecl.ID) {
    _check(decl: d, { (_, _) in () })
  }

  private mutating func check(associatedValue d: AssociatedValueDecl.ID) {
    _check(decl: d, { (_, _) in () })
  }

  /// Type checks `d` and returns its type.
  ///
  /// - Note: Method is internal because it may be called during constraint generation.
  @discardableResult
  mutating func check(binding d: BindingDecl.ID) -> AnyType {
    defer { assert(declTypes[d] != nil) }

    /// Registers that type checking completed for `d`, assiging it to `t` and returning `t`.
    func complete(_ t: AnyType) -> AnyType {
      assert(!t[.hasVariable])
      declTypes[d] = t
      declRequests[d] = .done
      return t
    }

    // Note: binding declarations do not undergo type realization.
    switch declRequests[d] {
    case nil:
      declRequests[d] = .typeCheckingStarted
    case .typeCheckingStarted:
      diagnostics.insert(.error(circularDependencyAt: ast[d].site))
      return complete(.error)
    case .done:
      return declTypes[d]!
    default:
      unreachable()
    }

    // Determine the shape of the declaration.
    let declScope = program.declToScope[AnyDeclID(d)]!
    let shape = inferredType(of: AnyPatternID(ast[d].pattern), in: declScope, shapedBy: nil)
    assert(shape.facts.inferredTypes.storage.isEmpty, "expression in binding pattern")

    if shape.type[.hasError] {
      return complete(.error)
    }

    // Determine whether the declaration has a type annotation.
    let hasTypeHint = ast[ast[d].pattern].annotation != nil

    // Type check the initializer, if any.
    if let initializer = ast[d].initializer {
      let initializerType = exprTypes[initializer].setIfNil(^TypeVariable())
      var initializerConstraints: [Constraint] = shape.facts.constraints

      // The type of the initializer may be a subtype of the pattern's
      if hasTypeHint {
        initializerConstraints.append(
          SubtypingConstraint(
            initializerType, shape.type,
            origin: ConstraintOrigin(.initializationWithHint, at: ast[initializer].site)))
      } else {
        initializerConstraints.append(
          EqualityConstraint(
            initializerType, shape.type,
            origin: ConstraintOrigin(.initializationWithPattern, at: ast[initializer].site)))
      }

      // Infer the type of the initializer
      let names = ast.names(in: ast[d].pattern).map({ AnyDeclID(ast[$0.pattern].decl) })

      bindingsUnderChecking.formUnion(names)
      let inference = solutionTyping(
        initializer,
        shapedBy: shape.type,
        in: declScope,
        initialConstraints: initializerConstraints)
      bindingsUnderChecking.subtract(names)

      // TODO: Complete underspecified generic signatures

      let result = complete(inference.solution.typeAssumptions.reify(shape.type))

      // Run deferred queries.
      let s = shape.deferred.reduce(true, { $1(&self, inference.solution) && $0 })
      assert(s || diagnostics.containsError)
      return result
    } else if hasTypeHint {
      return complete(shape.type)
    } else {
      unreachable("expected type annotation")
    }
  }

  private mutating func check(conformance d: ConformanceDecl.ID) {
    _check(decl: d, { (this, d) in this._check(conformance: d) })
  }

  private mutating func _check(conformance d: ConformanceDecl.ID) {
    let s = AnyScopeID(d)
    guard let receiver = realize(ast[d].subject, in: s)?.instance else { return }

    // Built-in types can't be extended.
    if let b = BuiltinType(receiver) {
      diagnostics.insert(.error(cannotExtend: b, at: ast[ast[d].subject].site))
      return
    }

    // TODO: Handle generics

    check(conformanceList: ast[d].conformances, partOf: d)
    check(all: ast[d].members)
  }

  private mutating func check(extension d: ExtensionDecl.ID) {
    _check(decl: d, { (this, d) in this._check(extension: d) })
  }

  private mutating func _check(extension d: ExtensionDecl.ID) {
    let s = AnyScopeID(d)
    guard let receiver = realize(ast[d].subject, in: s)?.instance else { return }

    // Built-in types can't be extended.
    if let b = BuiltinType(receiver) {
      diagnostics.insert(.error(cannotExtend: b, at: ast[ast[d].subject].site))
      return
    }

    // TODO: Handle generics

    check(all: ast[d].members)
  }

  /// Type checks the specified function declaration and returns whether that succeeded.
  ///
  /// The type of the declaration must be realizable from type annotations alone or the declaration
  /// the declaration must be realized and its inferred type must be stored in `declTyes`. Hence,
  /// the method must not be called on the underlying declaration of a lambda or spawn expression
  /// before the type of that declaration has been fully inferred.
  ///
  /// - SeeAlso: `checkPending`
  private mutating func check(function id: FunctionDecl.ID) {
    _check(decl: id, { (this, id) in this._check(function: id) })
  }

  private mutating func _check(function id: FunctionDecl.ID) {
    // Type check the generic constraints.
    _ = environment(of: id)

    // Type check the parameters.
    var parameterNames: Set<String> = []
    for parameter in ast[id].parameters {
      check(parameter: parameter, siblingNames: &parameterNames)
    }

    // Type check the body, if any.
    switch ast[id].body {
    case .block(let stmt):
      check(braceStmt: stmt)

    case .expr(let body):
      // If `expr` has been used to infer the return type, there's no need to visit it again.
      if (ast[id].output == nil) && ast[id].isInExprContext { return }

      // Inline functions may return `Never` regardless of their return type.
      let r = LambdaType(declTypes[id]!)!.output.skolemized
      let (t, c) = typeAndConstraintOfBody(body, inFunctionReturning: r)
      _ = solutionTyping(body, shapedBy: t, in: id, initialConstraints: [c])

    case nil:
      // Requirements and FFIs can be without a body.
      if program.isRequirement(id) || ast[id].isForeignInterface { return }

      // Declaration requires a body.
      diagnostics.insert(.error(declarationRequiresBodyAt: ast[id].introducerSite))
    }
  }

  /// Returns `(t, c)` where `t` is the type of the body `e` of a single-expression function whose
  /// return type is `r`, and `c` is the constraint placed on `t`.
  ///
  /// Use this method to create initial constraints passed to `solutionTyping` to type check the
  /// body of a single-expression function. The returned constraint allows this body to have type
  /// `Never` even if the function declares a different return type.
  private mutating func typeAndConstraintOfBody(
    _ e: AnyExprID, inFunctionReturning r: AnyType
  ) -> (AnyType, Constraint) {
    let t = exprTypes[e].setIfNil(^TypeVariable())
    let o = ConstraintOrigin(.return, at: ast[e].site)
    let constrainToNever = EqualityConstraint(t, .never, origin: o)

    if relations.areEquivalent(r, .never) {
      return (t, constrainToNever)
    } else {
      let c = DisjunctionConstraint(
        choices: [
          .init(constraints: [SubtypingConstraint(t, r, origin: o)], penalties: 0),
          .init(constraints: [constrainToNever], penalties: 1),
        ],
        origin: o)
      return (t, c)
    }
  }

  private mutating func check(genericParameter d: GenericParameterDecl.ID) {
    // TODO: Type check default values.
    _check(decl: d, { (_, _) in () })
  }

  private mutating func check(importDecl d: ImportDecl.ID) {
    _check(decl: d, { (_, _) in () })
  }

  private mutating func check(initializer d: InitializerDecl.ID) {
    _check(decl: d, { (this, d) in this._check(initializer: d) })
  }

  private mutating func _check(initializer d: InitializerDecl.ID) {
    // Memberwize initializers trivially type check.
    if ast[d].isMemberwise { return }

    // Type check the generic constraints.
    _ = environment(of: d)

    // Type check the parameters.
    var parameterNames: Set<String> = []
    for parameter in ast[d].parameters {
      check(parameter: parameter, siblingNames: &parameterNames)
    }

    // Set the type of the implicit receiver declaration.
    // Note: the receiver of an initializer is its first parameter.
    let type = LambdaType(declTypes[d]!)!
    declTypes[ast[d].receiver] = type.inputs[0].type
    declRequests[ast[d].receiver] = .typeRealizationCompleted

    // Type check the body, if any.
    if let body = ast[d].body {
      check(braceStmt: body)
    } else if !program.isRequirement(d) {
      diagnostics.insert(.error(declarationRequiresBodyAt: ast[d].introducer.site))
    }
  }

  private mutating func check(method d: MethodDecl.ID) {
    _check(decl: d, { (this, d) in this._check(method: d) })
  }

  private mutating func _check(method d: MethodDecl.ID) {
    // Type check the generic constraints.
    _ = environment(of: d)

    // Type check the parameters.
    var parameterNames: Set<String> = []
    for parameter in ast[d].parameters {
      check(parameter: parameter, siblingNames: &parameterNames)
    }

    // Type check the bodies.
    let bundle = MethodType(declTypes[d])!
    for v in ast[d].impls {
      declTypes[ast[v].receiver] = ^ParameterType(ast[v].introducer.value, bundle.receiver)
      declRequests[ast[v].receiver] = .done
      check(methodImpl: v)
    }
  }

  private mutating func check(methodImpl d: MethodImpl.ID) {
    switch ast[d].body {
    case .expr(let e):
      _ = checkedType(of: e, subtypeOf: LambdaType(declTypes[d])!.output, in: d)
    case .block(let s):
      check(braceStmt: s)
    case nil:
      if !program.isRequirement(d) {
        diagnostics.insert(.error(declarationRequiresBodyAt: ast[d].introducer.site))
      }
    }

    declRequests[d] = .done
  }

  /// Inserts in `siblingNames` the name of the parameter declaration identified by `d`.
  private mutating func check(
    parameter d: ParameterDecl.ID, siblingNames: inout Set<String>
  ) {
    // Check for duplicate parameter names.
    if !siblingNames.insert(ast[d].baseName).inserted {
      diagnostics.insert(.error(duplicateParameterNamed: ast[d].baseName, at: ast[d].site))
    }

    // Type check the default value, if any.
    if let defaultValue = ast[d].defaultValue {
      let parameterType = ParameterType(declTypes[d]!)!
      let defaultValueType = exprTypes[defaultValue].setIfNil(^TypeVariable())

      _ = solutionTyping(
        defaultValue,
        shapedBy: parameterType.bareType,
        in: program.declToScope[d]!,
        initialConstraints: [
          ParameterConstraint(
            defaultValueType, ^parameterType,
            origin: ConstraintOrigin(.argument, at: ast[d].site))
        ])
    }

    declRequests[d] = .typeRealizationCompleted
  }

  private mutating func check(namespace d: NamespaceDecl.ID) {
    _check(decl: d, { (this, d) in this._check(namespace: d) })
  }

  private mutating func _check(namespace d: NamespaceDecl.ID) {
    for m in ast[d].members {
      check(decl: m)
    }
  }

  private mutating func check(operator d: OperatorDecl.ID) {
    // Look for duplicate operator declaration.
    let source = TranslationUnit.ID(program.declToScope[d]!)!
    for decl in ast[source].decls where decl.kind == OperatorDecl.self {
      let oper = OperatorDecl.ID(decl)!
      if oper != d,
        ast[oper].notation.value == ast[d].notation.value,
        ast[oper].name.value == ast[d].name.value
      {
        diagnostics.insert(.error(duplicateOperatorNamed: ast[d].name.value, at: ast[d].site))
      }
    }
  }

  private mutating func check(productType d: ProductTypeDecl.ID) {
    _check(decl: d, { (this, d) in this._check(productType: d) })
  }

  private mutating func _check(productType d: ProductTypeDecl.ID) {
    _ = environment(of: d)
    check(initializer: ast[d].memberwiseInit)
    check(all: ast[d].members)
    check(conformanceList: ast[d].conformances, partOf: d)
  }

  private mutating func check(subscript d: SubscriptDecl.ID) {
    _check(decl: d, { (this, d) in this._check(subscript: d) })
  }

  private mutating func _check(subscript d: SubscriptDecl.ID) {
    // The type of the declaration must have been realized.
    let declType = SubscriptType(declTypes[d]!)!
    let outputType = declType.output.skolemized

    // Type check the generic constraints.
    _ = environment(of: d)

    // Type check the parameters, if any.
    if let parameters = ast[d].parameters {
      var parameterNames: Set<String> = []
      for parameter in parameters {
        check(parameter: parameter, siblingNames: &parameterNames)
      }
    }

    // Type checks the subscript's implementations.
    for v in ast[d].impls {
      if let receiver = ast[v].receiver {
        declTypes[receiver] = ^ParameterType(RemoteType(declType.captures.first!.type)!)
        declRequests[receiver] = .typeRealizationCompleted
      }
      check(subscriptImpl: v, outputType: outputType)
    }
  }

  private mutating func check(subscriptImpl d: SubscriptImpl.ID, outputType: AnyType) {
    switch ast[d].body {
    case .expr(let e):
      _ = checkedType(of: e, subtypeOf: outputType, in: d)
    case .block(let s):
      check(braceStmt: s)
    case nil:
      if !program.isRequirement(d) {
        diagnostics.insert(.error(declarationRequiresBodyAt: ast[d].introducer.site))
      }
    }

    declRequests[d] = .done
  }

  private mutating func check(trait d: TraitDecl.ID) {
    _check(decl: d, { (this, d) in this._check(trait: d) })
  }

  private mutating func _check(trait d: TraitDecl.ID) {
    guard let t = MetatypeType(declTypes[d]!)?.instance else { return }
    _ = environment(ofTraitDecl: d)
    check(all: ast[d].members)
    check(all: extendingDecls(of: t, exposedTo: program.declToScope[d]!))

    // TODO: Check refinements
  }

  private mutating func check(typeAlias d: TypeAliasDecl.ID) {
    _check(decl: d, { (this, id) in this._check(typeAlias: id) })
  }

  private mutating func _check(typeAlias d: TypeAliasDecl.ID) {
    guard let t = MetatypeType(declTypes[d]!)?.instance else { return }
    _ = environment(of: d)
    check(all: extendingDecls(of: t, exposedTo: program.declToScope[d]!))

    // TODO: Check conformances
  }

  /// Type checks `d` using `check` iff `d` hasn't been checked already.
  ///
  /// - Postcondition: `declRequests[d]` is `.typeCheckingCompleted`.
  private mutating func _check<T: DeclID>(decl d: T, _ check: (inout Self, T) -> Void) {
    if prepareForTypeChecking(d) != .done {
      check(&self, d)
      declRequests[d] = .done
    }
  }

  /// Ensures that the overarching type of `d` has been realized, returning its request status.
  private mutating func prepareForTypeChecking<T: DeclID>(_ d: T) -> RequestStatus {
    switch declRequests[d] {
    case nil:
      // Realize the type of the declaration before starting type checking.
      if realize(decl: d).isError {
        // Type checking fails if type realization did.
        declRequests[d] = .done
        return .done
      } else {
        // Note: Because the type realization of certain declarations may escalate to type
        // checking perform type checking, we should re-check the status of the request.
        return prepareForTypeChecking(d)
      }

    case .typeRealizationCompleted:
      declRequests[d] = .typeCheckingStarted
      return .typeCheckingStarted

    case .typeRealizationStarted, .typeCheckingStarted:
      diagnostics.insert(.error(circularDependencyAt: ast[d].site))
      declRequests[d] = .done
      return .done

    case .done:
      return .done
    }
  }

  /// Type check the conformance list `traits` that's part of declaration `d`.
  private mutating func check<T: Decl & LexicalScope>(
    conformanceList traits: [NameExpr.ID], partOf d: T.ID
  ) {
    let receiver = realizeSelfTypeExpr(in: d)!.instance
    let declContainer = program.scopeToParent[d]!
    for e in traits {
      guard let rhs = realize(name: e, in: declContainer)?.instance else { continue }
      guard rhs.base is TraitType else {
        diagnostics.insert(.error(conformanceToNonTraitType: rhs, at: ast[e].site))
        continue
      }

      for t in conformedTraits(of: rhs, in: declContainer) {
        checkAndRegisterConformance(of: receiver, to: t, declaredBy: d, at: ast[e].site)
      }
    }
  }

  /// Registers the conformance of `model` to `trait` declared by `source` in `self.relations` if
  /// it is satisfied. Otherwise, reports diagnostics at `declSite`.
  private mutating func checkAndRegisterConformance<T: Decl & LexicalScope>(
    of model: AnyType,
    to trait: TraitType,
    declaredBy source: T.ID,
    at declSite: SourceRange
  ) {
    guard let c = checkConformance(of: model, to: trait, declaredBy: source, at: declSite)
    else {
      // Diagnostics have been reported by `checkConformance`.
      return
    }

    let (inserted, x) = relations.insert(c, testingContainmentWith: program)
    if !inserted {
      diagnostics.insert(.error(redundantConformance: c, at: declSite, alreadyDeclaredAt: x.site))
    }
  }

  /// Returns the conformance of `model` to `trait` declared by `source` if it's satisfied.
  /// Otherwise, reports missing requirements at `declSite` and returns `nil`.
  private mutating func checkConformance<T: Decl & LexicalScope>(
    of model: AnyType,
    to trait: TraitType,
    declaredBy source: T.ID,
    at declSite: SourceRange
  ) -> Conformance? {
    let useScope = AnyScopeID(source)
    let specializations: GenericArguments = [ast[trait.decl].selfParameterDecl: model]
    var implementations = Conformance.ImplementationMap()
    var notes: DiagnosticSet = []

    // Get the set of generic parameters defined by `trait`.
    for m in ast[trait.decl].members {
      switch m.kind {
      case GenericParameterDecl.self:
        assert(m == ast[trait.decl].selfParameterDecl, "unexpected declaration")
        continue

      case AssociatedTypeDecl.self:
        // TODO: Implement me.
        continue

      case AssociatedValueDecl.self:
        // TODO: Implement me.
        continue

      case FunctionDecl.self:
        checkSatisfied(function: .init(m)!)

      case InitializerDecl.self:
        checkSatisfied(initializer: .init(m)!)

      case MethodDecl.self:
        let r = MethodDecl.ID(m)!
        let n = Name(of: r, in: ast)
        ast[r].impls.forEach({ checkSatisfied(variant: $0, inMethod: n) })

      case SubscriptDecl.self:
        // TODO: Implement me.
        continue

      default:
        unreachable()
      }
    }

    if notes.containsError {
      diagnostics.insert(.error(model, doesNotConformTo: trait, at: declSite, because: notes))
      return nil
    }

    // Conformances at file scope are exposed in the whole module. Other conformances are exposed
    // in their containing scope.
    let expositionScope = read(program.scopeToParent[source]!) { (s) in
      (s.kind == TranslationUnit.self) ? AnyScopeID(program.module(containing: s)) : s
    }

    return Conformance(
      model: model, concept: trait, conditions: [],
      source: AnyDeclID(source), scope: expositionScope,
      implementations: implementations,
      site: declSite)

    /// Checks if requirement `d` is satisfied by `model`, extending `implementations` if it is or
    /// reporting a diagnostic in `notes` otherwise.
    func checkSatisfied(initializer d: InitializerDecl.ID) {
      let requiredType = relations.canonical(
        specialized(realize(decl: d), applying: specializations, in: useScope))
      guard !requiredType[.hasError] else { return }

      if let c = implementation(
        of: Name(of: d, in: ast), in: model,
        withCallableType: LambdaType(requiredType)!, specializedWith: specializations,
        exposedTo: useScope)
      {
        implementations[d] = .concrete(c)
      } else {
        notes.insert(.error(trait: trait, requiresInitializer: requiredType, at: declSite))
      }
    }

    /// Checks if requirement `d` is satisfied by `model`, extending `implementations` if it is or
    /// reporting a diagnostic in `notes` otherwise.
    func checkSatisfied(function d: FunctionDecl.ID) {
      let requiredType = specialized(realize(decl: d), applying: specializations, in: useScope)
      guard !requiredType[.hasError] else { return }

      let t = relations.canonical(requiredType)
      let requiredName = Name(of: d, in: ast)!
      if let c = implementation(
        of: requiredName, in: model,
        withCallableType: LambdaType(t)!, specializedWith: specializations,
        exposedTo: useScope)
      {
        implementations[d] = .concrete(c)
      } else if let i = synthesizedImplementation(of: d, for: t, in: useScope) {
        implementations[d] = .synthetic(t)
        synthesizedDecls[program.module(containing: d), default: []].append(i)
      } else {
        notes.insert(
          .error(trait: trait, requiresMethod: requiredName, withType: requiredType, at: declSite))
      }
    }

    /// Checks if requirement `d` of a method bunde named `m` is satisfied by `model`, extending
    /// `implementations` if it is or reporting a diagnostic in `notes` otherwise.
    func checkSatisfied(variant d: MethodImpl.ID, inMethod m: Name) {
      let requiredType = specialized(realize(decl: d), applying: specializations, in: useScope)
      guard !requiredType[.hasError] else { return }

      let t = relations.canonical(requiredType)
      if let c = implementation(
        of: m, in: model,
        withCallableType: LambdaType(t)!, specializedWith: specializations,
        exposedTo: useScope)
      {
        implementations[d] = .concrete(c)
      } else if let i = synthesizedImplementation(of: d, for: t, in: useScope) {
        implementations[d] = .synthetic(t)
        synthesizedDecls[program.module(containing: d), default: []].append(i)
      } else {
        let requiredName = m.appending(ast[d].introducer.value)!
        notes.insert(
          .error(trait: trait, requiresMethod: requiredName, withType: requiredType, at: declSite))
      }
    }
  }

  /// Returns the declaration exposed to `scope` of a callable member in `model` that introduces
  /// `requirementName` with type `requiredType`, using `specializations` to subsititute
  /// associated types and values. Returns `nil` if zero or more than 1 candidates were found.
  private mutating func implementation(
    of requirementName: Name,
    in model: AnyType,
    withCallableType requiredType: LambdaType,
    specializedWith specializations: GenericArguments,
    exposedTo scope: AnyScopeID
  ) -> AnyDeclID? {
    /// Returns `true` if candidate `d` has `requirementType`.
    func hasRequiredType<T: Decl>(_ d: T.ID) -> Bool {
      relations.areEquivalent(
        specialized(realize(decl: d), applying: specializations, in: scope),
        ^requiredType)
    }

    let allCandidates = lookup(requirementName.stem, memberOf: model, exposedTo: scope)
    let viableCandidates = allCandidates.compactMap { (c) -> AnyDeclID? in

      // TODO: Filter out the candidates with incompatible constraints.
      // trait A {}
      // type Foo<T> {}
      // extension Foo where T: U { fun foo() }
      // conformance Foo: A {} // <- should not consider `foo` in the extension

      switch c.kind {
      case FunctionDecl.self:
        let d = FunctionDecl.ID(c)!
        return ((ast[d].body != nil) && hasRequiredType(d)) ? c : nil

      case InitializerDecl.self:
        let d = InitializerDecl.ID(c)!
        return ((ast[d].body != nil) && hasRequiredType(d)) ? c : nil

      case MethodDecl.self:
        for d in ast[MethodDecl.ID(c)!].impls where ast[d].body != nil {
          if hasRequiredType(d) { return c }
        }
        return nil

      default:
        return nil
      }
    }

    if viableCandidates.count > 1 {
      // TODO: Rank candidates
      fatalError("not implemented")
    }

    return viableCandidates.uniqueElement
  }

  /// Returns the synthesized implementation of requirement `r` for type `t` in given `scope`, or
  /// `nil` if `r` is not synthesizable.
  private func synthesizedImplementation<T: DeclID>(
    of r: T, for t: AnyType, in scope: AnyScopeID
  ) -> SynthesizedDecl? {
    guard let s = program.innermostType(containing: r).map(TraitDecl.ID.init(_:)) else {
      return nil
    }

    // If the requirement is defined in `Sinkable`, it must be either the move-initialization or
    // move-assignment method.
    if s == ast.sinkableTrait.decl {
      let d = MethodImpl.ID(r)!
      switch ast[d].introducer.value {
      case .set:
        return .init(.moveInitialization, for: t, in: scope)
      case .inout:
        return .init(.moveAssignment, for: t, in: scope)
      default:
        unreachable()
      }
    }

    // If the requirement is defined in `Copyable`, it must me the copy method.
    if s == ast.copyableTrait.decl {
      assert(r.kind == FunctionDecl.self)
      return .init(.copy, for: t, in: scope)
    }

    // Requirement is not synthesizable.
    return nil
  }

  /// Returns an array of declarations implementing `requirement` with type `requirementType` that
  /// are member of `conformingType` and exposed in `scope`.
  private mutating func gatherCandidates(
    implementing requirement: MethodDecl.ID,
    withType requirementType: AnyType,
    for conformingType: AnyType,
    exposedTo scope: AnyScopeID
  ) -> [AnyDeclID] {
    let n = Name(of: requirement, in: ast)
    let lookupResult = lookup(n.stem, memberOf: conformingType, exposedTo: scope)

    // Filter out the candidates with incompatible types.
    return lookupResult.compactMap { (c) -> AnyDeclID? in
      guard
        c != requirement,
        let d = self.decl(in: c, named: n),
        relations.canonical(realize(decl: d)) == requirementType
      else { return nil }

      if let f = MethodDecl.ID(d) {
        if ast[f].impls.contains(where: ({ ast[$0].body == nil })) { return nil }
      }

      // TODO: Filter out the candidates with incompatible constraints.
      // trait A {}
      // type Foo<T> {}
      // extension Foo where T: U { fun foo() }
      // conformance Foo: A {} // <- should not consider `foo` in the extension

      // TODO: Rank candidates

      return d
    }
  }

  /// Type checks `s`.
  private mutating func check<T: StmtID>(stmt s: T, in scope: AnyScopeID) {
    switch s.kind {
    case AssignStmt.self:
      check(assign: NodeID(s)!, in: scope)
    case BraceStmt.self:
      check(braceStmt: NodeID(s)!)
    case ConditionalStmt.self:
      check(conditional: NodeID(s)!, in: scope)
    case ExprStmt.self:
      check(exprStmt: NodeID(s)!, in: scope)
    case DeclStmt.self:
      check(decl: ast[DeclStmt.ID(s)!].decl)
    case DiscardStmt.self:
      _ = checkedType(of: ast[DiscardStmt.ID(s)!].expr, in: scope)
    case DoWhileStmt.self:
      check(doWhile: NodeID(s)!, in: scope)
    case ReturnStmt.self:
      check(return: NodeID(s)!, in: scope)
    case WhileStmt.self:
      check(while: NodeID(s)!, in: scope)
    case YieldStmt.self:
      check(yield: NodeID(s)!, in: scope)
    case ForStmt.self, BreakStmt.self, ContinueStmt.self:
      // TODO: implement checks for these statements
      break
    default:
      unexpected(s, in: ast)
    }
  }

  /// Type checks `s`.
  ///
  /// - Note: Method is internal because it may be called during constraint generation.
  mutating func check(braceStmt s: BraceStmt.ID) {
    let context = AnyScopeID(s)
    ast[s].stmts.forEach({ (x) in check(stmt: x, in: context) })
  }

  private mutating func check(assign s: AssignStmt.ID, in scope: AnyScopeID) {
    // Target type must be `Sinkable`.
    guard let targetType = checkedType(of: ast[s].left, in: scope) else { return }
    let lhsConstraint = ConformanceConstraint(
      targetType, conformsTo: [ast.coreTrait("Sinkable")!],
      origin: ConstraintOrigin(.initializationOrAssignment, at: ast[s].site))

    // Source type must be subtype of the target type.
    let sourceType = exprTypes[ast[s].right].setIfNil(^TypeVariable())
    let rhsConstraint = SubtypingConstraint(
      sourceType, targetType,
      origin: ConstraintOrigin(.initializationOrAssignment, at: ast[s].site))

    // Note: Type information flows strictly from left to right.
    _ = solutionTyping(
      ast[s].right, shapedBy: targetType, in: scope,
      initialConstraints: [lhsConstraint, rhsConstraint])
  }

  private mutating func check(conditional s: ConditionalStmt.ID, in scope: AnyScopeID) {
    let boolType = AnyType(ast.coreType("Bool")!)
    for c in ast[s].condition {
      switch c {
      case .expr(let e):
        _ = checkedType(of: e, subtypeOf: boolType, in: scope)
      default:
        fatalError("not implemented")
      }
    }

    check(braceStmt: ast[s].success)
    if let b = ast[s].failure {
      check(stmt: b, in: scope)
    }
  }

  private mutating func check(exprStmt s: ExprStmt.ID, in scope: AnyScopeID) {
    guard let result = checkedType(of: ast[s].expr, in: scope) else { return }

    // Warn against unused result if the type of the expression is neither `Void` nor `Never`.
    let t = relations.canonical(result)
    if (t != .void) && (t != .never) {
      diagnostics.insert(.warning(unusedResultOfType: result, at: ast[ast[s].expr].site))
    }
  }

  private mutating func check(doWhile subject: DoWhileStmt.ID, in scope: AnyScopeID) {
    check(braceStmt: ast[subject].body)

    // Visit the condition of the loop in the scope of the body.
    let boolType = AnyType(ast.coreType("Bool")!)
    check(ast[subject].condition, in: ast[subject].body, hasType: boolType, cause: .structural)
  }

  private mutating func check(return id: ReturnStmt.ID, in scope: AnyScopeID) {
    let o = expectedOutputType(in: scope)!
    if let v = ast[id].value {
      _ = checkedType(of: v, subtypeOf: o, in: scope)
    } else if !relations.areEquivalent(o, .void) {
      diagnostics.insert(.error(missingReturnValueAt: ast[id].site))
    }
  }

  private mutating func check(while s: WhileStmt.ID, in scope: AnyScopeID) {
    // Visit the condition(s).
    let boolType = AnyType(ast.coreType("Bool")!)
    for item in ast[s].condition {
      switch item {
      case .expr(let e):
        // Condition must be Boolean.
        check(e, in: scope, hasType: boolType, cause: .structural)
      case .decl(let binding):
        check(binding: binding)
      }
    }

    // Visit the body.
    check(braceStmt: ast[s].body)
  }

  private mutating func check(yield s: YieldStmt.ID, in scope: AnyScopeID) {
    let o = expectedOutputType(in: scope)!
    _ = checkedType(of: ast[s].value, subtypeOf: o, in: scope)
  }

  /// Returns whether `d` is well-typed, reading type inference results from `s`.
  mutating func checkDeferred(varDecl d: VarDecl.ID, _ s: Solution) -> Bool {
    let s = modify(&declTypes[d]!) { (t) in
      // TODO: Diagnose reification failures
      t = s.typeAssumptions.reify(t)
      return !t[.hasError]
    }
    declRequests[d] = .done
    return s
  }

  /// Returns whether `e` is well-typed, reading type inference results from `s`.
  mutating func checkDeferred(lambdaExpr e: LambdaExpr.ID, _ s: Solution) -> Bool {
    // TODO: Diagnose reification failures
    guard
      let declType = exprTypes[e]?.base as? LambdaType,
      !declType[.hasError]
    else { return false }

    // Reify the type of the underlying declaration.
    declTypes[ast[e].decl] = ^declType
    let parameters = ast[ast[e].decl].parameters
    for i in 0 ..< parameters.count {
      declTypes[parameters[i]] = declType.inputs[i].type
    }

    // Type check the declaration.
    check(function: ast[e].decl)
    return !declType[.hasError]
  }

  /// Returns the expected output type in `lexicalContext`, or `nil` if `lexicalContext` is not
  /// nested in a function or subscript declaration.
  private func expectedOutputType<S: ScopeID>(in lexicalContext: S) -> AnyType? {
    for s in program.scopes(from: lexicalContext) {
      switch s.kind {
      case MethodImpl.self:
        return LambdaType(declTypes[MethodImpl.ID(s)!])?.output.skolemized
      case FunctionDecl.self:
        return LambdaType(declTypes[FunctionDecl.ID(s)!])?.output.skolemized
      case SubscriptDecl.self:
        return SubscriptType(declTypes[SubscriptDecl.ID(s)!])?.output.skolemized
      default:
        continue
      }
    }

    return nil
  }

  /// Returns the generic environment defined by `scope`.
  ///
  /// - Requires: `scope` denotes a generic lexical scope.
  private mutating func environment<T: NodeIDProtocol>(of scope: T) -> GenericEnvironment {
    switch scope.kind {
    case FunctionDecl.self:
      return environment(of: FunctionDecl.ID(scope)!)
    case ProductTypeDecl.self:
      return environment(of: ProductTypeDecl.ID(scope)!)
    case SubscriptDecl.self:
      return environment(of: SubscriptDecl.ID(scope)!)
    case TypeAliasDecl.self:
      return environment(of: TypeAliasDecl.ID(scope)!)
    case TraitDecl.self:
      return environment(ofTraitDecl: NodeID(scope)!)
    default:
      unreachable()
    }
  }

  /// Returns the generic environment defined by `id`.
  private mutating func environment<T: GenericDecl>(of id: T.ID) -> GenericEnvironment {
    assert(T.self != TraitDecl.self, "trait environements use a more specialized method")

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
      let e = GenericEnvironment(decl: id, parameters: [], constraints: [], into: &self)
      environments[id] = .done(e)
      return e
    }

    var constraints: [Constraint] = []

    // Check the conformance list of each generic type parameter.
    for p in clause.parameters {
      // Realize the parameter's declaration.
      let parameterType = realize(genericParameterDecl: p)
      if parameterType.isError { continue }

      // TODO: Type check default values.

      // Skip value declarations.
      guard
        let lhs = MetatypeType(parameterType)?.instance,
        lhs.base is GenericTypeParameterType
      else { continue }

      // Synthesize the sugared conformance constraint, if any.
      let rhs = ast[p].conformances
      let requiredTraits = realize(conformances: rhs, in: program.scopeToParent[AnyScopeID(id)!]!)
      if !requiredTraits.isEmpty {
        let constraintSite = ast[p].identifier.site
        constraints.append(
          ConformanceConstraint(
            lhs, conformsTo: requiredTraits, origin: .init(.annotation, at: constraintSite)))
      }
    }

    // Evaluate the constraint expressions of the associated type's where clause.
    if let whereClause = clause.whereClause?.value {
      for expr in whereClause.constraints {
        if let constraint = eval(constraintExpr: expr, in: AnyScopeID(id)!) {
          constraints.append(constraint)
        }
      }
    }

    let e = GenericEnvironment(
      decl: id, parameters: clause.parameters, constraints: constraints, into: &self)
    environments[id] = .done(e)
    return e
  }

  /// Returns the generic environment defined by `i`, or `nil` if it is ill-typed.
  private mutating func environment<T: TypeExtendingDecl>(
    ofTypeExtendingDecl id: T.ID
  ) -> GenericEnvironment {
    switch environments[id] {
    case .done(let e):
      return e
    case .inProgress:
      fatalError("circular dependency")
    case nil:
      environments[id] = .inProgress
    }

    let scope = AnyScopeID(id)
    var constraints: [Constraint] = []

    // Evaluate the constraint expressions of the associated type's where clause.
    if let whereClause = ast[id].whereClause?.value {
      for expr in whereClause.constraints {
        if let constraint = eval(constraintExpr: expr, in: scope) {
          constraints.append(constraint)
        }
      }
    }

    let e = GenericEnvironment(decl: id, parameters: [], constraints: constraints, into: &self)
    environments[id] = .done(e)
    return e
  }

  /// Returns the generic environment defined by `i`, or `nil` if it is ill-typed.
  private mutating func environment(
    ofTraitDecl id: TraitDecl.ID
  ) -> GenericEnvironment {
    switch environments[id] {
    case .done(let e):
      return e
    case .inProgress:
      fatalError("circular dependency")
    case nil:
      environments[id] = .inProgress
    }

    var constraints: [Constraint] = []

    // Collect and type check the constraints defined on associated types and values.
    for member in ast[id].members {
      switch member.kind {
      case AssociatedTypeDecl.self:
        appendAssociatedTypeConstraints(of: NodeID(member)!, declaredIn: id, to: &constraints)
      case AssociatedValueDecl.self:
        appendAssociatedValueConstraints(of: NodeID(member)!, declaredIn: id, to: &constraints)
      default:
        continue
      }
    }

    // Synthesize `Self: T`.
    let selfDecl = ast[id].selfParameterDecl
    let selfType = GenericTypeParameterType(selfDecl, ast: ast)
    let declaredTrait = TraitType(MetatypeType(declTypes[id]!)!.instance)!
    constraints.append(
      ConformanceConstraint(
        ^selfType, conformsTo: [declaredTrait],
        origin: ConstraintOrigin(.structural, at: ast[id].identifier.site)))

    let e = GenericEnvironment(
      decl: id, parameters: [selfDecl], constraints: constraints, into: &self)
    environments[id] = .done(e)
    return e
  }

  /// Evaluates the valid constraints declared in `associatedType` and adds them to `constraints`.
  private mutating func appendAssociatedTypeConstraints(
    of associatedType: AssociatedTypeDecl.ID,
    declaredIn trait: TraitDecl.ID,
    to constraints: inout [Constraint]
  ) {
    // Realize the LHS of the constraint.
    let lhs = realize(decl: associatedType)
    if lhs.isError { return }

    // Synthesize the sugared conformance constraint, if any.
    let rhs = ast[associatedType].conformances
    let requiredTraits = realize(conformances: rhs, in: AnyScopeID(trait))
    if !requiredTraits.isEmpty {
      let constraintSite = ast[associatedType].identifier.site
      constraints.append(
        ConformanceConstraint(
          lhs, conformsTo: requiredTraits, origin: .init(.annotation, at: constraintSite)))
    }

    // Evaluate the constraint expressions of the associated type's where clause.
    if let whereClause = ast[associatedType].whereClause?.value {
      for expr in whereClause.constraints {
        if let constraint = eval(constraintExpr: expr, in: AnyScopeID(trait)) {
          constraints.append(constraint)
        }
      }
    }
  }

  /// Evaluates the valid constraints declared in `associatedValue` and adds them to `constraints`.
  private mutating func appendAssociatedValueConstraints(
    of associatedValue: AssociatedValueDecl.ID,
    declaredIn trait: TraitDecl.ID,
    to constraints: inout [Constraint]
  ) {
    // Realize the LHS of the constraint.
    if realize(decl: associatedValue).isError { return }

    // Evaluate the constraint expressions of the associated value's where clause.
    if let whereClause = ast[associatedValue].whereClause?.value {
      for expr in whereClause.constraints {
        if let constraint = eval(constraintExpr: expr, in: AnyScopeID(trait)) {
          constraints.append(constraint)
        }
      }
    }
  }

  /// Evaluates `expr` in `scope` and returns a type constraint, or `nil` if evaluation failed.
  ///
  /// - Note: Calling this method multiple times with the same arguments may duplicate diagnostics.
  private mutating func eval(
    constraintExpr expr: SourceRepresentable<WhereClause.ConstraintExpr>,
    in scope: AnyScopeID
  ) -> Constraint? {
    switch expr.value {
    case .equality(let l, let r):
      guard let a = realize(name: l, in: scope)?.instance else { return nil }
      guard let b = realize(r, in: scope)?.instance else { return nil }

      if !a.isTypeParam && !b.isTypeParam {
        diagnostics.insert(.error(invalidEqualityConstraintBetween: a, and: b, at: expr.site))
        return nil
      }

      return EqualityConstraint(a, b, origin: ConstraintOrigin(.structural, at: expr.site))

    case .conformance(let l, let traits):
      guard let a = realize(name: l, in: scope)?.instance else { return nil }
      if !a.isTypeParam {
        diagnostics.insert(.error(invalidConformanceConstraintTo: a, at: expr.site))
        return nil
      }

      var b: Set<TraitType> = []
      for i in traits {
        guard let type = realize(name: i, in: scope)?.instance else { return nil }
        if let trait = type.base as? TraitType {
          b.insert(trait)
        } else {
          diagnostics.insert(.error(conformanceToNonTraitType: a, at: expr.site))
          return nil
        }
      }

      return ConformanceConstraint(
        a, conformsTo: b, origin: ConstraintOrigin(.structural, at: expr.site))

    case .value(let e):
      // TODO: Symbolic execution
      return PredicateConstraint(e, origin: ConstraintOrigin(.structural, at: expr.site))
    }
  }

  // MARK: Type inference

  /// Checks that `e`, which occurs in `scope`,  has type `t` due to `c`.
  private mutating func check<S: ScopeID>(
    _ e: AnyExprID, in scope: S, hasType t: AnyType, cause c: ConstraintOrigin.Kind
  ) {
    let u = exprTypes[e].setIfNil(^TypeVariable())
    _ = solutionTyping(
      e, shapedBy: t, in: scope,
      initialConstraints: [EqualityConstraint(u, t, origin: .init(c, at: ast[e].site))])
  }

  /// Returns the type of `subject` knowing it occurs in `scope` and is shaped by `shape`, or `nil`
  /// if such type couldn't be deduced.
  ///
  /// - Parameters:
  ///   - subject: The expression whose type should be deduced.
  ///   - shape: The shape of the type `subject` is expected to have given top-bottom information
  ///     flow, or `nil` of such shape is unknown.
  ///   - scope: The innermost scope containing `subject`.
  private mutating func checkedType<S: ScopeID>(
    of subject: AnyExprID,
    subtypeOf supertype: AnyType? = nil,
    in scope: S
  ) -> AnyType? {
    var c: [Constraint] = []
    if let t = supertype {
      let u = exprTypes[subject].setIfNil(^TypeVariable())
      c.append(SubtypingConstraint(u, t, origin: .init(.structural, at: ast[subject].site)))
    }

    let i = solutionTyping(subject, shapedBy: supertype, in: scope, initialConstraints: c)
    return i.succeeded ? exprTypes[subject]! : nil
  }

  /// Returns the best solution satisfying `initialConstraints` and describing the types of
  /// `subject` and its sub-expressions, knowing `subject` occurs in `scope` and is shaped by
  /// `shape`.
  ///
  /// - Parameters:
  ///   - subject: The expression whose constituent types should be deduced.
  ///   - shape: The shape of the type `subject` is expected to have given top-bottom information
  ///     flow, or `nil` of such shape is unknown.
  ///   - scope: The innermost scope containing `subject`.
  ///   - initialConstraints: A collection of constraints on constituent types of `subject`.
  mutating func solutionTyping<S: ScopeID>(
    _ subject: AnyExprID,
    shapedBy shape: AnyType?,
    in scope: S,
    initialConstraints: [Constraint] = []
  ) -> (succeeded: Bool, solution: Solution) {
    // Determine whether tracing should be enabled.
    let shouldLogTrace: Bool
    if let tracingSite = inferenceTracingSite,
      tracingSite.bounds.contains(ast[subject].site.first())
    {
      let subjectSite = ast[subject].site
      shouldLogTrace = true
      let loc = subjectSite.first()
      let subjectDescription = subjectSite.file[subjectSite]
      print("Inferring type of '\(subjectDescription)' at \(loc)")
      print("---")
    } else {
      shouldLogTrace = false
    }

    // Generate constraints.
    let (_, facts, deferredQueries) = inferredType(
      of: subject, shapedBy: shape, in: AnyScopeID(scope))

    // Bail out if constraint generation failed.
    if facts.foundConflict {
      return (succeeded: false, solution: .init())
    }

    // Solve the constraints.
    var s = ConstraintSystem(
      initialConstraints + facts.constraints, in: AnyScopeID(scope),
      loggingTrace: shouldLogTrace)
    let solution = s.solution(&self)

    if shouldLogTrace {
      print(solution)
    }

    // Apply the solution.
    for (id, type) in facts.inferredTypes.storage {
      exprTypes[id] = solution.typeAssumptions.reify(type)
    }
    for (name, ref) in solution.bindingAssumptions {
      referredDecls[name] = ref
    }

    // Run deferred queries.
    let success = deferredQueries.reduce(
      !solution.diagnostics.containsError, { (s, q) in q(&self, solution) && s })

    diagnostics.formUnion(solution.diagnostics)
    assert(success || diagnostics.containsError)
    return (succeeded: success, solution: solution)
  }

  // MARK: Name binding

  /// The result of a name lookup.
  public typealias DeclSet = Set<AnyDeclID>

  /// A lookup table.
  private typealias LookupTable = [String: DeclSet]

  private struct MemberLookupKey: Hashable {

    var type: AnyType

    var scope: AnyScopeID

  }

  /// The member lookup tables of the types.
  ///
  /// This property is used to memoize the results of `lookup(_:memberOf:in)`.
  private var memberLookupTables: [MemberLookupKey: LookupTable] = [:]

  /// A set containing the type extending declarations being currently bounded.
  ///
  /// This property is used during conformance and extension binding to avoid infinite recursion
  /// through qualified lookups into the extended type.
  private var extensionsUnderBinding = DeclSet()

  /// Returns `(head, tail)` where `head` contains the nominal components of `name` from right to
  /// left and `tail` is the non-nominal component of `name`, if any.
  ///
  /// Name expressions are rperesented as linked-list, whose elements are the components of a
  /// name in reverse order. This method splits such lists at the first non-nominal component.
  private func splitNominalComponents(of name: NameExpr.ID) -> ([NameExpr.ID], NameExpr.Domain?) {
    var suffix = [name]
    while true {
      let d = ast[suffix.last!].domain
      switch d {
      case .none:
        return (suffix, nil)
      case .implicit:
        return (suffix, d)
      case .expr(let e):
        guard let p = NameExpr.ID(e) else { return (suffix, d) }
        suffix.append(p)
      }
    }
  }

  /// Resolves the non-overloaded name components of `name` from left to right in `scope`.
  ///
  /// - Postcondition: If the method returns `.done(resolved: r, unresolved: u)`, `r` is not empty
  ///   and `r[i].candidates` has a single element for `0 < i < r.count`.
  mutating func resolveNominalPrefix(
    of name: NameExpr.ID,
    in scope: AnyScopeID
  ) -> NameResolutionResult {
    var (unresolved, domain) = splitNominalComponents(of: name)
    if domain != nil {
      return .inexecutable(unresolved)
    }

    // Resolve the nominal components of `nameExpr` from left to right as long as we don't need
    // contextual information to resolve overload sets.
    var resolved: [NameResolutionResult.ResolvedComponent] = []
    var parent: (type: AnyType, arguments: GenericArguments)? = nil

    while let component = unresolved.popLast() {
      // Evaluate the static argument list.
      var arguments: [AnyType] = []
      for a in ast[component].arguments {
        guard let type = realize(a.value, in: scope)?.instance else { return .failed }
        arguments.append(type)
      }

      // Resolve the component.
      let n = ast[component].name
      let candidates = resolve(n, parameterizedBy: arguments, memberOf: parent, exposedTo: scope)

      if candidates.elements.isEmpty {
        report(.error(undefinedName: n.value, in: parent?.type, at: n.site))
        return .failed
      }

      if candidates.viable.isEmpty {
        let notes = candidates.elements.compactMap(\.argumentsDiagnostic)
        report(.error(noViableCandidateToResolve: n, notes: notes))
        return .failed
      }

      // Append the resolved component to the nominal prefix.
      let selected = candidates.viable.map({ candidates.elements[$0] })
      resolved.append(.init(component, selected))

      // Defer resolution of the remaining name components if there are multiple candidates for
      // the current component or if we found a type variable.
      if (selected.count > 1) || (selected[0].type.shape.base is TypeVariable) { break }
      let c = selected[0]

      // If the candidate is a direct reference to a type declaration, the next component should be
      // looked up in the referred type's declaration space rather than that of its metatype.
      if let d = c.reference.decl, isNominalTypeDecl(d) {
        parent = (MetatypeType(c.type.shape)!.instance, c.reference.arguments)
      } else {
        parent = (c.type.shape, c.reference.arguments)
      }
    }

    precondition(!resolved.isEmpty)
    return .done(resolved: resolved, unresolved: unresolved)
  }

  /// Returns the declarations of `name` exposed to `useScope` and parameterized by `arguments`.
  ///
  /// The declarations are searched with an unqualified lookup unless `parent` is set, in which
  /// case they are searched in its declaration space. Generic candidates are specialized with
  /// the generic arguments of `parent` if it has any.
  mutating func resolve(
    _ name: SourceRepresentable<Name>,
    memberOf parent: AnyType,
    exposedTo useScope: AnyScopeID
  ) -> NameResolutionResult.CandidateSet {
    let p = BoundGenericType(parent).map({ ($0.base, $0.arguments) }) ?? (parent, [:])
    return resolve(name, memberOf: p, exposedTo: useScope)
  }

  /// Returns the declarations of `name` exposed to `useScope` and parameterized by `arguments`.
  ///
  /// The declarations are searched with an unqualified lookup unless `parent` is set, in which
  /// case they are searched in the declaration space of `parent.type`. Generic candidates are
  /// specialized with `arguments` appended to `parent.arguments`.
  private mutating func resolve(
    _ name: SourceRepresentable<Name>,
    parameterizedBy arguments: [any CompileTimeValue] = [],
    memberOf parent: (type: AnyType, arguments: GenericArguments)?,
    exposedTo useScope: AnyScopeID
  ) -> NameResolutionResult.CandidateSet {
    // Resolve references to the built-in symbols.
    if isBuiltinModuleVisible {
      if (parent == nil) && (name.value.stem == "Builtin") {
        return [.init(.module)]
      }
      if parent?.type == .builtin(.module) {
        return resolve(builtin: name)
      }
    }

    // Gather declarations qualified by `parent` if it isn't `nil` or unqualified otherwise.
    let matches = lookup(name, memberOf: parent?.type, exposedTo: useScope)
    if matches.isEmpty { return [] }

    // Create declaration references to all candidates.
    var candidates: NameResolutionResult.CandidateSet = []
    let parentArguments = parent?.arguments ?? [:]
    for m in matches {
      var argumentsDiagnostic: Diagnostic? = nil
      guard var matchType = resolvedType(of: m) else { continue }
      guard
        var matchArguments = associateGenericParameters(
          of: name, declaredBy: m, to: arguments, reportingDiagnosticTo: &argumentsDiagnostic)
      else { continue }

      if let g = BoundGenericType(matchType) {
        assert(matchArguments.isEmpty, "generic declaration bound twice")
        matchArguments = g.arguments
      }

      let allArguments = parentArguments.appending(matchArguments)
      matchType = bind(matchType, to: allArguments)
      matchType = specialized(matchType, applying: allArguments, in: useScope)

      let t = instantiate(
        matchType, in: program.scopeIntroducing(m),
        cause: .init(.binding, at: name.site))

      let r: DeclReference
      if program.isNonStaticMember(m) && !(parent?.type.base is MetatypeType) {
        r = .member(m, allArguments)
      } else {
        r = .direct(m, allArguments)
      }
      candidates.insert(.init(reference: r, type: t, argumentsDiagnostic: argumentsDiagnostic))
    }

    return candidates
  }

  /// Returns the resolved type of the entity declared by `d` or `nil` if is invalid.
  private mutating func resolvedType(of d: AnyDeclID) -> AnyType? {
    var result = realize(decl: d)
    if result.isError { return nil }

    // Properties are not first-class.
    if let s = SubscriptDecl.ID(d), ast[s].isProperty {
      result = SubscriptType(result)!.output
    }

    // Erase parameter conventions.
    if let t = ParameterType(result) {
      result = t.bareType
    }

    return result
  }

  /// Resolves a reference to the built-in type or function named `name`.
  private mutating func resolve(
    builtin name: SourceRepresentable<Name>
  ) -> NameResolutionResult.CandidateSet {
    if let f = BuiltinFunction(name.value.stem) {
      return [.init(f)]
    }
    if let t = BuiltinType(name.value.stem) {
      return [.init(t)]
    }
    return []
  }

  /// Returns a sequence of key-value pairs associating the generic parameters introduced by `d`,
  /// which declares `name`, to corresponding value in `arguments`, or `nil` if such an argument
  /// list doesn't match `d`'s generic parameters.
  private mutating func associateGenericParameters(
    of name: SourceRepresentable<Name>,
    declaredBy d: AnyDeclID,
    to arguments: [any CompileTimeValue],
    reportingDiagnosticTo argumentsDiagnostic: inout Diagnostic?
  ) -> GenericArguments? {
    if arguments.isEmpty { return [:] }

    guard d.kind.value is GenericScope.Type else {
      argumentsDiagnostic =
        .error(invalidGenericArgumentCountTo: name, found: arguments.count, expected: 0)
      return nil
    }

    let parameters = environment(of: d).parameters
    guard parameters.count == arguments.count else {
      let (f, e) = (arguments.count, parameters.count)
      argumentsDiagnostic = .error(invalidGenericArgumentCountTo: name, found: f, expected: e)
      return nil
    }

    argumentsDiagnostic = nil
    return .init(uniqueKeysWithValues: zip(parameters, arguments))
  }

  /// Returns the declarations exposing a name with given `stem` to `useScope` without
  /// qualification.
  mutating func lookup(unqualified baseName: String, in useScope: AnyScopeID) -> DeclSet {
    var matches = DeclSet()
    var containingFile: TranslationUnit.ID? = nil
    var containingModule: ModuleDecl.ID? = nil

    for s in program.scopes(from: useScope) {
      if let u = TranslationUnit.ID(s) {
        containingFile = u
      } else if let m = ModuleDecl.ID(s) {
        containingModule = m
      }

      // Gather declarations of the identifier in the current scope; we can assume we've got no
      // no non-overloadable candidate.
      let newMatches = lookup(baseName, introducedInDeclSpaceOf: s, exposedTo: useScope)
        .subtracting(bindingsUnderChecking)
      for d in newMatches {
        if let result = matches.inserting(d) { return result }
      }
    }

    // Handle references to the containing module.
    if ast[containingModule]?.baseName == baseName {
      if let result = matches.inserting(containingModule!) { return result }
    }

    // Handle references to imported symbols.
    if let u = containingFile, let fileImports = imports[u] {
      for m in fileImports {
        matches.formUnion(names(introducedIn: m)[baseName, default: []])
      }
    }

    return matches
  }

  /// Returns the declarations introducing a name with given `stem` in the declaration space of
  /// `lookupContext` and exposed to `useScope`.
  private mutating func lookup<T: ScopeID>(
    _ stem: String,
    introducedInDeclSpaceOf lookupContext: T,
    exposedTo useScope: AnyScopeID
  ) -> DeclSet {
    switch lookupContext.kind {
    case ProductTypeDecl.self:
      let t = ^ProductType(NodeID(lookupContext)!, ast: ast)
      return lookup(stem, memberOf: t, exposedTo: useScope)

    case TraitDecl.self:
      let t = ^TraitType(NodeID(lookupContext)!, ast: ast)
      return lookup(stem, memberOf: t, exposedTo: useScope)

    case ConformanceDecl.self:
      let d = ConformanceDecl.ID(lookupContext)!
      if let t = MetatypeType(realize(typeExtendingDecl: d))?.instance {
        return t.isError ? [] : lookup(stem, memberOf: t, exposedTo: useScope)
      } else {
        return names(introducedIn: d)[stem, default: []]
      }

    case ExtensionDecl.self:
      let d = ExtensionDecl.ID(lookupContext)!
      if let t = MetatypeType(realize(typeExtendingDecl: d))?.instance {
        return t.isError ? [] : lookup(stem, memberOf: t, exposedTo: useScope)
      } else {
        return names(introducedIn: d)[stem, default: []]
      }

    case TypeAliasDecl.self:
      // We can't re-enter `realize(typeAliasDecl:)` if the aliased type of `d` is being resolved
      // but its generic parameters can be lookep up already.
      let d = TypeAliasDecl.ID(lookupContext)!
      if declRequests[d] == .typeRealizationStarted {
        return names(introducedIn: d)[stem, default: []]
      }

      if let t = MetatypeType(realize(typeAliasDecl: d))?.instance {
        return t.isError ? [] : lookup(stem, memberOf: t, exposedTo: useScope)
      } else {
        return []
      }

    default:
      return names(introducedIn: lookupContext)[stem, default: []]
    }
  }

  /// Returns the declarations introducing a name with given `stem` as a member of `domain` and
  /// exposed to `useScope`.
  mutating func lookup(
    _ stem: String,
    memberOf domain: AnyType,
    exposedTo useScope: AnyScopeID
  ) -> DeclSet {
    switch domain.base {
    case let t as BoundGenericType:
      return lookup(stem, memberOf: t.base, exposedTo: useScope)
    case let t as ConformanceLensType:
      return lookup(stem, memberOf: ^t.lens, exposedTo: useScope)
    default:
      break
    }

    let key = MemberLookupKey(type: domain, scope: useScope)
    if let m = memberLookupTables[key]?[stem] {
      return m
    }

    var matches: DeclSet
    defer { memberLookupTables[key, default: [:]][stem] = matches }

    switch domain.base {
    case let t as ProductType:
      matches = names(introducedIn: t.decl)[stem, default: []]
    case let t as ModuleType:
      matches = names(introducedIn: t.decl)[stem, default: []]
    case let t as NamespaceType:
      matches = names(introducedIn: t.decl)[stem, default: []]
    case let t as TraitType:
      matches = names(introducedIn: t.decl)[stem, default: []]
    case let t as TypeAliasType:
      matches = names(introducedIn: t.decl)[stem, default: []]
    default:
      matches = DeclSet()
    }

    // Look for members declared in extensions.
    for i in extendingDecls(of: domain, exposedTo: useScope) {
      matches.formUnion(names(introducedIn: i)[stem, default: []])
    }

    // Look for members declared inherited by conformance/refinement.
    for trait in conformedTraits(of: domain, in: useScope) where domain != trait {
      // TODO: Read source of conformance to disambiguate associated names
      let newMatches = lookup(stem, memberOf: ^trait, exposedTo: useScope)

      // Associated type and value declarations are not inherited by conformance. Traits do not
      // inherit the generic parameters.
      switch domain.base {
      case is AssociatedTypeType, is GenericTypeParameterType:
        matches.formUnion(newMatches)
      case is TraitType:
        matches.formUnion(newMatches.filter({ $0.kind != GenericParameterDecl.self }))
      default:
        matches.formUnion(newMatches.filter(program.isRequirement(_:)))
      }
    }

    return matches
  }

  /// Returns the declarations introducing `name` in the declaration space that are exposed to
  /// `useScope` and are member of `parentType` unless it is `nil`.
  private mutating func lookup(
    _ name: SourceRepresentable<Name>,
    memberOf parentType: AnyType?,
    exposedTo useScope: AnyScopeID
  ) -> [AnyDeclID] {
    if let t = parentType {
      return lookup(name.value.stem, memberOf: t, exposedTo: useScope)
        .compactMap({ decl(in: $0, named: name.value) })
    } else {
      return lookup(unqualified: name.value.stem, in: useScope)
        .compactMap({ decl(in: $0, named: name.value) })
    }
  }

  /// Returns the declaration(s) of the specified operator that are visible in `useScope`.
  func lookup(
    operator operatorName: Identifier,
    notation: OperatorNotation,
    exposedTo useScope: AnyScopeID
  ) -> [OperatorDecl.ID] {
    let currentModule = program.module(containing: useScope)
    if let oper = lookup(operator: operatorName, notation: notation, in: currentModule) {
      return [oper]
    }

    return ast.modules.compactMap({ (module) -> OperatorDecl.ID? in
      if module == currentModule { return nil }
      return lookup(operator: operatorName, notation: notation, in: module)
    })
  }

  /// Returns the declaration of the specified operator in `module`, if any.
  func lookup(
    operator operatorName: Identifier,
    notation: OperatorNotation,
    in module: ModuleDecl.ID
  ) -> OperatorDecl.ID? {
    for decl in ast.topLevelDecls(module) where decl.kind == OperatorDecl.self {
      let oper = OperatorDecl.ID(decl)!
      if (ast[oper].notation.value == notation) && (ast[oper].name.value == operatorName) {
        return oper
      }
    }
    return nil
  }

  /// Returns the extending declarations of `subject` visible in `useScope`.
  ///
  /// - Note: The declarations referred by the returned IDs conform to `TypeExtendingDecl`.
  private mutating func extendingDecls<S: ScopeID>(
    of subject: AnyType,
    exposedTo useScope: S
  ) -> [AnyDeclID] {
    /// The canonical form of `subject`.
    let canonicalSubject = relations.canonical(subject)
    /// The declarations extending `subject`.
    var matches: [AnyDeclID] = []
    /// The module at the root of `useScope`, when found.
    var root: ModuleDecl.ID? = nil

    // Look for extension declarations in all visible scopes.
    for scope in program.scopes(from: useScope) {
      switch scope.kind {
      case ModuleDecl.self:
        let m = ModuleDecl.ID(scope)!
        let symbols = ast.topLevelDecls(m)
        insert(into: &matches, decls: symbols, extending: canonicalSubject, in: scope)
        root = m

      case TranslationUnit.self:
        continue

      default:
        let decls = program.scopeToDecls[scope, default: []]
        insert(into: &matches, decls: decls, extending: canonicalSubject, in: scope)
      }
    }

    // Nowhere else to look if `useScope` is a module.
    if useScope.kind == ModuleDecl.self { return matches }

    // Look for extension declarations in imported modules.
    let imports = self.imports[program.source(containing: useScope), default: []]
    for m in imports where m != root {
      let symbols = ast.topLevelDecls(m)
      insert(into: &matches, decls: symbols, extending: canonicalSubject, in: AnyScopeID(m))
    }

    return matches
  }

  /// Insert into `matches` the declarations in `decls` that extend `subject` in `scope`.
  ///
  /// - Requires: `subject` must be canonical.
  private mutating func insert<S: Sequence>(
    into matches: inout [AnyDeclID],
    decls: S,
    extending subject: AnyType,
    in scope: AnyScopeID
  ) where S.Element == AnyDeclID {
    precondition(subject[.isCanonical])

    for i in decls where i.kind == ConformanceDecl.self || i.kind == ExtensionDecl.self {
      // Skip extending declarations that are being bound.
      guard extensionsUnderBinding.insert(i).inserted else { continue }
      defer { extensionsUnderBinding.remove(i) }

      // Check for matches.
      guard let extendedType = realize(decl: i).base as? MetatypeType else { continue }
      if relations.canonical(extendedType.instance) == subject {
        matches.append(i)
      }
    }
  }

  /// Returns the names and declarations introduced in `scope`.
  private func names<T: NodeIDProtocol>(introducedIn scope: T) -> LookupTable {
    if let module = ModuleDecl.ID(scope) {
      return ast[module].sources.reduce(into: [:]) { (table, s) in
        table.merge(names(introducedIn: s), uniquingKeysWith: { (l, _) in l })
      }
    }

    guard let decls = program.scopeToDecls[scope] else { return [:] }
    var table: LookupTable = [:]

    for id in decls {
      switch id.kind {
      case AssociatedValueDecl.self,
        AssociatedTypeDecl.self,
        GenericParameterDecl.self,
        ImportDecl.self,
        NamespaceDecl.self,
        ParameterDecl.self,
        ProductTypeDecl.self,
        TraitDecl.self,
        TypeAliasDecl.self,
        VarDecl.self:
        let name = (ast[id] as! SingleEntityDecl).baseName
        table[name, default: []].insert(id)

      case BindingDecl.self,
        ConformanceDecl.self,
        ExtensionDecl.self,
        MethodImpl.self,
        OperatorDecl.self,
        SubscriptImpl.self:
        // Note: operator declarations are not considered during standard name lookup.
        break

      case FunctionDecl.self:
        guard let i = ast[FunctionDecl.ID(id)!].identifier?.value else { continue }
        table[i, default: []].insert(id)

      case InitializerDecl.self:
        table["init", default: []].insert(id)

      case MethodDecl.self:
        table[ast[MethodDecl.ID(id)!].identifier.value, default: []].insert(id)

      case SubscriptDecl.self:
        let i = ast[SubscriptDecl.ID(id)!].identifier?.value ?? "[]"
        table[i, default: []].insert(id)

      default:
        unexpected(id, in: ast)
      }
    }

    // Note: Results should be memoized.
    return table
  }

  // MARK: Type realization

  /// Realizes and returns the type denoted by `expr` evaluated in `scope`.
  mutating func realize(_ expr: AnyExprID, in scope: AnyScopeID) -> MetatypeType? {
    switch expr.kind {
    case ConformanceLensTypeExpr.self:
      return realize(conformanceLens: NodeID(expr)!, in: scope)
    case ExistentialTypeExpr.self:
      return realize(existentialType: NodeID(expr)!, in: scope)
    case LambdaTypeExpr.self:
      return realize(lambda: NodeID(expr)!, in: scope)
    case NameExpr.self:
      return realize(name: NodeID(expr)!, in: scope)
    case TupleTypeExpr.self:
      return realize(tuple: NodeID(expr)!, in: scope)
    case WildcardExpr.self:
      return MetatypeType(of: TypeVariable())
    default:
      unexpected(expr, in: ast)
    }
  }

  /// Returns the realized type of the function declaration underlying `expr` requiring that its
  /// parameters have the given `conventions`.
  ///
  /// - Requires: if supplied, `conventions` has as one element per parameter of the declaration
  ///   underlying `expr`.
  mutating func realize(
    underlyingDeclOf expr: LambdaExpr.ID,
    with conventions: [AccessEffect]?
  ) -> AnyType? {
    realize(functionDecl: ast[expr].decl, with: conventions)
  }

  /// Realizes and returns a "magic" type expression.
  private mutating func realizeMagicTypeExpr(
    _ expr: NameExpr.ID,
    in scope: AnyScopeID
  ) -> MetatypeType? {
    precondition(ast[expr].domain == .none)

    // Determine the "magic" type expression to realize.
    let name = ast[expr].name
    switch name.value.stem {
    case "Sum":
      return realizeSumTypeExpr(expr, in: scope)
    default:
      break
    }

    // Evaluate the static argument list.
    var arguments: [(value: any CompileTimeValue, site: SourceRange)] = []
    for a in ast[expr].arguments {
      // TODO: Symbolic execution
      guard let v = realize(a.value, in: scope)?.instance else { return nil }
      arguments.append((value: v, site: ast[a.value].site))
    }

    switch name.value.stem {
    case "Any":
      let type = MetatypeType(of: .any)
      if arguments.count > 0 {
        diagnostics.insert(.error(argumentToNonGenericType: type.instance, at: name.site))
        return nil
      }
      return type

    case "Never":
      let type = MetatypeType(of: .never)
      if arguments.count > 0 {
        diagnostics.insert(.error(argumentToNonGenericType: type.instance, at: name.site))
        return nil
      }
      return type

    case "Self":
      guard let type = realizeSelfTypeExpr(in: scope) else {
        diagnostics.insert(.error(invalidReferenceToSelfTypeAt: name.site))
        return nil
      }
      if arguments.count > 0 {
        diagnostics.insert(.error(argumentToNonGenericType: type.instance, at: name.site))
        return nil
      }
      return type

    case "Metatype":
      if arguments.count > 1 {
        diagnostics.insert(
          .error(invalidGenericArgumentCountTo: name, found: arguments.count, expected: 1))
        return nil
      }
      if let a = arguments.first {
        let instance = (a.value as? AnyType) ?? fatalError("not implemented")
        return MetatypeType(of: MetatypeType(of: instance))
      } else {
        return MetatypeType(of: MetatypeType(of: TypeVariable()))
      }

    case "Builtin" where isBuiltinModuleVisible:
      let type = MetatypeType(of: .builtin(.module))
      if arguments.count > 0 {
        diagnostics.insert(.error(argumentToNonGenericType: type.instance, at: name.site))
        return nil
      }
      return type

    default:
      diagnostics.insert(.error(noType: name.value, in: nil, at: name.site))
      return nil
    }
  }

  /// Returns the type of a sum type expression with the given arguments.
  ///
  /// - Requires: `sumTypeExpr` is a sum type expression.
  private mutating func realizeSumTypeExpr(
    _ sumTypeExpr: NameExpr.ID,
    in scope: AnyScopeID
  ) -> MetatypeType? {
    precondition(ast[sumTypeExpr].name.value.stem == "Sum")

    var elements = SumType.Elements()
    for a in ast[sumTypeExpr].arguments {
      guard let type = realize(a.value, in: scope)?.instance else {
        diagnostics.insert(.error(valueInSumTypeAt: ast[a.value].site))
        return nil
      }
      elements.insert(type)
    }

    switch elements.count {
    case 0:
      diagnostics.insert(.warning(sumTypeWithZeroElementsAt: ast[sumTypeExpr].name.site))
      return MetatypeType(of: .never)

    case 1:
      diagnostics.insert(.error(sumTypeWithOneElementAt: ast[sumTypeExpr].name.site))
      return nil

    default:
      return MetatypeType(of: SumType(elements))
    }
  }

  /// Realizes and returns the type of the `Self` expression in `scope`.
  ///
  /// - Note: This method does not issue diagnostics.
  private mutating func realizeSelfTypeExpr<T: ScopeID>(in scope: T) -> MetatypeType? {
    for scope in program.scopes(from: scope) {
      switch scope.kind {
      case TraitDecl.self:
        let decl = TraitDecl.ID(scope)!
        return MetatypeType(of: GenericTypeParameterType(selfParameterOf: decl, in: ast))

      case ProductTypeDecl.self:
        // Synthesize unparameterized `Self`.
        let decl = ProductTypeDecl.ID(scope)!
        let unparameterized = ProductType(decl, ast: ast)

        // Synthesize arguments to generic parameters if necessary.
        if let parameters = ast[decl].genericClause?.value.parameters {
          let arguments = GenericArguments(
            uniqueKeysWithValues: parameters.map({ (p) in
              (key: p, value: ^GenericTypeParameterType(p, ast: ast))
            }))
          return MetatypeType(of: BoundGenericType(unparameterized, arguments: arguments))
        } else {
          return MetatypeType(of: unparameterized)
        }

      case ConformanceDecl.self:
        let decl = ConformanceDecl.ID(scope)!
        return realize(ast[decl].subject, in: scope)

      case ExtensionDecl.self:
        let decl = ExtensionDecl.ID(scope)!
        return realize(ast[decl].subject, in: scope)

      case TypeAliasDecl.self:
        fatalError("not implemented")

      default:
        continue
      }
    }

    return nil
  }

  private mutating func realize(
    conformanceLens id: ConformanceLensTypeExpr.ID,
    in scope: AnyScopeID
  ) -> MetatypeType? {
    let node = ast[id]

    /// The lens must be a trait.
    guard let lens = realize(node.lens, in: scope)?.instance else { return nil }
    guard let lensTrait = lens.base as? TraitType else {
      diagnostics.insert(.error(notATrait: lens, at: ast[node.lens].site))
      return nil
    }

    // The subject must conform to the lens.
    guard let subject = realize(node.subject, in: scope)?.instance else { return nil }
    if !conformedTraits(of: subject, in: scope).contains(lensTrait) {
      diagnostics.insert(.error(subject, doesNotConformTo: lensTrait, at: ast[node.lens].site))
      return nil
    }

    return MetatypeType(of: ConformanceLensType(viewing: subject, through: lensTrait))
  }

  private mutating func realize(
    existentialType e: ExistentialTypeExpr.ID,
    in scope: AnyScopeID
  ) -> MetatypeType? {
    assert(!ast[e].traits.isEmpty, "existential type with no interface")

    // Realize the interface.
    var interface: [AnyType] = []
    for n in ast[e].traits {
      guard let i = realize(name: n, in: scope)?.instance else { return nil }
      interface.append(i)
    }

    // TODO: Process where clauses
    guard ast[e].whereClause == nil else { fatalError("not implemented") }

    // Interface must be either a single type or a set of traits.
    if let t = TraitType(interface[0]) {
      var traits = Set([t])
      for i in 1 ..< interface.count {
        if let u = TraitType(interface[i]) {
          traits.insert(u)
        } else {
          diagnostics.insert(.error(notATrait: interface[i], at: ast[ast[e].traits[i]].site))
          return nil
        }
      }

      return MetatypeType(of: ExistentialType(traits: traits, constraints: []))
    } else if let t = interface.uniqueElement {
      return MetatypeType(of: ExistentialType(unparameterized: t, constraints: []))
    } else {
      diagnostics.insert(.error(tooManyExistentialBoundsAt: ast[ast[e].traits[1]].site))
      return nil
    }
  }

  private mutating func realize(
    lambda id: LambdaTypeExpr.ID,
    in scope: AnyScopeID
  ) -> MetatypeType? {
    let node = ast[id]

    // Realize the lambda's environment.
    let environment: AnyType
    if let environmentExpr = node.environment {
      guard let ty = realize(environmentExpr, in: scope) else { return nil }
      environment = ty.instance
    } else {
      environment = .any
    }

    // Realize the lambda's parameters.
    var inputs: [CallableTypeParameter] = []
    inputs.reserveCapacity(node.parameters.count)

    for p in node.parameters {
      guard let ty = realize(parameter: p.type, in: scope)?.instance else { return nil }
      inputs.append(.init(label: p.label?.value, type: ty))
    }

    // Realize the lambda's output.
    guard let output = realize(node.output, in: scope)?.instance else { return nil }

    return MetatypeType(
      of: LambdaType(
        receiverEffect: node.receiverEffect?.value ?? .let,
        environment: environment,
        inputs: inputs,
        output: output))
  }

  private mutating func realize(
    name id: NameExpr.ID,
    in scope: AnyScopeID
  ) -> MetatypeType? {
    // Note: This function has become pretty hacky but it may not be worth refactoring before we
    // tackle the evaluation of compile-time expressions properly.

    let name = ast[id].name
    let domain: AnyType?
    let matches: DeclSet

    // Realize the name's domain, if any.
    switch ast[id].domain {
    case .none:
      // Name expression has no domain; gather declarations with an unqualified lookup or try to
      // evaluate the name as a "magic" type expression if there are no candidates.
      domain = nil
      matches = lookup(unqualified: name.value.stem, in: scope)
      if matches.isEmpty {
        return realizeMagicTypeExpr(id, in: scope)
      }

    case .expr(let j):
      // Hack to handle cases where the domain refers to a module.
      if let n = NameExpr.ID(j), ast[n].domain == .none,
        let d = lookup(unqualified: ast[n].name.value.stem, in: scope).uniqueElement,
        let t = ModuleType(realize(decl: d))
      {
        domain = ^t
      } else if let t = realize(j, in: scope)?.instance {
        domain = t
      } else {
        return nil
      }

      // Handle references to built-in types.
      if relations.areEquivalent(domain!, .builtin(.module)) {
        if let t = BuiltinType(name.value.stem) {
          return MetatypeType(of: .builtin(t))
        } else {
          diagnostics.insert(.error(noType: name.value, in: domain, at: name.site))
          return nil
        }
      }

      // Gather declarations with a qualified lookup.
      matches = lookup(name.value.stem, memberOf: domain!, exposedTo: scope)

    case .implicit:
      diagnostics.insert(.error(notEnoughContextToResolveMember: name))
      return nil
    }

    // Diagnose unresolved names.
    guard let match = matches.first else {
      diagnostics.insert(.error(noType: name.value, in: domain, at: name.site))
      return nil
    }

    // Diagnose ambiguous references.
    if matches.count > 1 {
      diagnostics.insert(.error(ambiguousUse: id, in: ast))
      return nil
    }

    // Realize the referred type.
    let referredType: MetatypeType
    if match.kind == AssociatedTypeDecl.self {
      let decl = AssociatedTypeDecl.ID(match)!

      switch domain?.base {
      case is AssociatedTypeType,
        is ConformanceLensType,
        is GenericTypeParameterType:
        referredType = MetatypeType(of: AssociatedTypeType(decl, domain: domain!, ast: ast))

      case nil:
        // Assume that `Self` in `scope` resolves to an implicit generic parameter of a trait
        // declaration, since associated declarations cannot be looked up unqualified outside
        // the scope of a trait and its extensions.
        let domain = realizeSelfTypeExpr(in: scope)!.instance
        let instance = AssociatedTypeType(NodeID(match)!, domain: domain, ast: ast)
        referredType = MetatypeType(of: instance)

      case .some:
        diagnostics.insert(.error(invalidUseOfAssociatedType: ast[decl].baseName, at: name.site))
        return nil
      }
    } else {
      switch realize(decl: match).base {
      case let t as MetatypeType:
        referredType = t
      default:
        diagnostics.insert(.error(nameRefersToValue: id, in: ast))
        return nil
      }
    }

    if ast[id].arguments.isEmpty {
      return referredType
    }

    guard let clause = (ast[match] as? GenericDecl)?.genericClause?.value else {
      diagnostics.insert(
        .error(
          "non-generic type '\(referredType.instance)' has no generic parameters",
          at: ast[ast[id].arguments[0].value].site))
      return nil
    }

    guard ast[id].arguments.count == clause.parameters.count else {
      diagnostics.insert(
        .error(
          invalidGenericArgumentCountTo: ast[id].name,
          found: ast[id].arguments.count, expected: clause.parameters.count))
      return nil
    }

    var arguments: GenericArguments = [:]
    for (p, a) in zip(clause.parameters, ast[id].arguments) {
      // TODO: Symbolic execution
      guard let v = realize(a.value, in: scope)?.instance else { return nil }
      arguments[p] = v
    }
    return MetatypeType(of: BoundGenericType(referredType.instance, arguments: arguments))
  }

  private mutating func realize(
    parameter id: ParameterTypeExpr.ID,
    in scope: AnyScopeID
  ) -> MetatypeType? {
    let node = ast[id]

    guard let bareType = realize(node.bareType, in: scope)?.instance else { return nil }
    return MetatypeType(of: ParameterType(node.convention.value, bareType))
  }

  private mutating func realize(
    tuple id: TupleTypeExpr.ID,
    in scope: AnyScopeID
  ) -> MetatypeType? {
    var elements: [TupleType.Element] = []
    elements.reserveCapacity(ast[id].elements.count)

    for e in ast[id].elements {
      guard let ty = realize(e.type, in: scope)?.instance else { return nil }
      elements.append(.init(label: e.label?.value, type: ty))
    }

    return MetatypeType(of: TupleType(elements))
  }

  /// Realizes and returns the traits denoted by each valid trait expression in `conformances`.
  private mutating func realize(
    conformances: [NameExpr.ID],
    in scope: AnyScopeID
  ) -> Set<TraitType> {
    var result: Set<TraitType> = []
    for expr in conformances {
      guard let rhs = realize(name: expr, in: scope)?.instance else { continue }
      if let t = TraitType(rhs) {
        result.insert(t)
      } else {
        diagnostics.insert(.error(conformanceToNonTraitType: rhs, at: ast[expr].site))
      }
    }
    return result
  }

  /// Returns the overarching type of `d`.
  mutating func realize<T: DeclID>(decl d: T) -> AnyType {
    switch d.kind {
    case AssociatedTypeDecl.self:
      return realize(associatedTypeDecl: NodeID(d)!)
    case AssociatedValueDecl.self:
      return realize(associatedValueDecl: NodeID(d)!)
    case GenericParameterDecl.self:
      return realize(genericParameterDecl: NodeID(d)!)
    case BindingDecl.self:
      return realize(bindingDecl: NodeID(d)!)
    case ConformanceDecl.self:
      return realize(typeExtendingDecl: ConformanceDecl.ID(d)!)
    case ExtensionDecl.self:
      return realize(typeExtendingDecl: ExtensionDecl.ID(d)!)
    case FunctionDecl.self:
      return realize(functionDecl: NodeID(d)!)
    case ImportDecl.self:
      return realize(importDecl: NodeID(d)!)
    case InitializerDecl.self:
      return realize(initializerDecl: NodeID(d)!)
    case MethodDecl.self:
      return realize(methodDecl: NodeID(d)!)
    case MethodImpl.self:
      return realize(methodImpl: NodeID(d)!)
    case ModuleDecl.self:
      return realize(moduleDecl: NodeID(d)!)
    case NamespaceDecl.self:
      return realize(namespaceDecl: NodeID(d)!)
    case ParameterDecl.self:
      return realize(parameterDecl: NodeID(d)!)
    case ProductTypeDecl.self:
      return realize(productTypeDecl: NodeID(d)!)
    case SubscriptDecl.self:
      return realize(subscriptDecl: NodeID(d)!)
    case TraitDecl.self:
      return realize(traitDecl: NodeID(d)!)
    case TypeAliasDecl.self:
      return realize(typeAliasDecl: NodeID(d)!)
    case VarDecl.self:
      return realize(varDecl: NodeID(d)!)
    default:
      unexpected(d, in: ast)
    }
  }

  /// Returns the overarching type of `d`.
  private mutating func realize(associatedTypeDecl d: AssociatedTypeDecl.ID) -> AnyType {
    _realize(decl: d) { (this, d) in
      // Parent scope must be a trait declaration.
      let traitDecl = TraitDecl.ID(this.program.declToScope[d]!)!

      let instance = AssociatedTypeType(
        NodeID(d)!,
        domain: ^GenericTypeParameterType(selfParameterOf: traitDecl, in: this.ast),
        ast: this.ast)
      return ^MetatypeType(of: instance)
    }
  }

  /// Returns the overarching type of `d`.
  private mutating func realize(associatedValueDecl d: AssociatedValueDecl.ID) -> AnyType {
    _realize(decl: d) { (this, d) in
      // Parent scope must be a trait declaration.
      let traitDecl = TraitDecl.ID(this.program.declToScope[d]!)!

      let instance = AssociatedValueType(
        NodeID(d)!,
        domain: ^GenericTypeParameterType(selfParameterOf: traitDecl, in: this.ast),
        ast: this.program.ast)
      return ^MetatypeType(of: instance)
    }
  }

  /// Returns the overarching type of `d`.
  private mutating func realize(bindingDecl d: BindingDecl.ID) -> AnyType {
    _ = check(binding: NodeID(d)!)
    return declTypes[d]!
  }

  /// Returns the overarching type of `d`, requiring that its parameters have given `conventions`.
  ///
  /// - Requires: if supplied, `conventions` has as many elements as `d` has parameters.
  private mutating func realize(
    functionDecl d: FunctionDecl.ID,
    with conventions: [AccessEffect]? = nil
  ) -> AnyType {
    _realize(decl: d, { (this, d) in this._realize(functionDecl: d, with: conventions) })
  }

  private mutating func _realize(
    functionDecl d: FunctionDecl.ID,
    with conventions: [AccessEffect]? = nil
  ) -> AnyType {
    // Realize the input types.
    var inputs: [CallableTypeParameter] = []
    for (i, p) in ast[d].parameters.enumerated() {
      let t: AnyType
      if ast[p].annotation != nil {
        t = realize(parameterDecl: p)
      } else if ast[d].isInExprContext {
        // Annotations may be elided in lambda expressions. In that case, unannotated parameters
        // are given a fresh type variable so that inference can proceed.
        t = ^ParameterType(conventions?[i] ?? .let, ^TypeVariable())
        declTypes[p] = t
        declRequests[p] = .typeRealizationCompleted
      } else {
        unreachable("expected type annotation")
      }

      let i = CallableTypeParameter(
        label: ast[p].label?.value,
        type: t,
        hasDefault: ast[p].defaultValue != nil)
      inputs.append(i)
    }

    // Collect captures.
    var explicitCaptureNames: Set<Name> = []
    guard
      let explicitCaptureTypes = realize(
        explicitCaptures: ast[d].explicitCaptures,
        collectingNamesIn: &explicitCaptureNames)
    else { return .error }

    let implicitCaptures: [ImplicitCapture] =
      program.isLocal(d)
      ? realize(implicitCapturesIn: d, ignoring: explicitCaptureNames)
      : []
    self.implicitCaptures[d] = implicitCaptures

    // Realize the output type.
    let output: AnyType
    if let o = ast[d].output {
      // Use the explicit return annotation.
      guard let type = realize(o, in: AnyScopeID(d))?.instance else { return .error }
      output = type
    } else if ast[d].isInExprContext {
      // Infer the return type from the body in expression contexts.
      output = ^TypeVariable()
    } else {
      // Default to `Void`.
      output = .void
    }

    if program.isNonStaticMember(d) {
      let effect = ast[d].receiverEffect?.value ?? .let
      let receiver = realizeSelfTypeExpr(in: program.declToScope[d]!)!.instance
      declTypes[ast[d].receiver!] = ^ParameterType(effect, receiver)
      declRequests[ast[d].receiver!] = .typeRealizationCompleted

      let e: TupleType
      switch effect {
      case .let, .inout, .set:
        e = TupleType([.init(label: "self", type: ^RemoteType(effect, receiver))])
      case .sink:
        e = TupleType([.init(label: "self", type: receiver)])
      case .yielded:
        unreachable()
      }

      return ^LambdaType(receiverEffect: effect, environment: ^e, inputs: inputs, output: output)
    } else {
      let e = TupleType(
        explicitCaptureTypes.map({ (t) in TupleType.Element(label: nil, type: t) })
          + implicitCaptures.map({ (c) in TupleType.Element(label: nil, type: ^c.type) }))

      // TODO: Determine if the lambda is mutating.

      return ^LambdaType(environment: ^e, inputs: inputs, output: output)
    }
  }

  /// Returns the overarching type of `d`.
  public mutating func realize(genericParameterDecl d: GenericParameterDecl.ID) -> AnyType {
    _realize(decl: d, { (this, d) in this._realize(genericParameterDecl: d) })
  }

  private mutating func _realize(genericParameterDecl d: GenericParameterDecl.ID) -> AnyType {
    // The declaration introduces a generic *type* parameter the first annotation refers to a
    // trait. Otherwise, it denotes a generic *value* parameter.
    if let annotation = ast[d].conformances.first {
      // Bail out if we can't evaluate the annotation.
      guard let type = realize(name: annotation, in: program.declToScope[d]!) else {
        return .error
      }

      if !(type.instance.base is TraitType) {
        // Value parameters shall not have more than one type annotation.
        if ast[d].conformances.count > 1 {
          let diagnosticOrigin = ast[ast[d].conformances[1]].site
          diagnostics.insert(
            .error(tooManyAnnotationsOnGenericValueParametersAt: diagnosticOrigin))
          return .error
        }

        // The declaration introduces a generic value parameter.
        return type.instance
      }
    }

    // If the declaration has no annotations or its first annotation does not refer to a trait,
    // assume it declares a generic type parameter.
    let instance = GenericTypeParameterType(d, ast: ast)
    return ^MetatypeType(of: instance)
  }

  /// Returns the overarching type of `d`.
  private mutating func realize(importDecl d: ImportDecl.ID) -> AnyType {
    _realize(decl: d, { (this, d) in this._realize(importDecl: d) })
  }

  private mutating func _realize(importDecl d: ImportDecl.ID) -> AnyType {
    guard let m = ast.modules.first(where: { ast[$0].baseName == ast[d].baseName }) else {
      diagnostics.insert(.error(noSuchModule: ast[d].baseName, at: ast[d].identifier.site))
      return .error
    }
    return ^ModuleType(m, ast: ast)
  }

  /// Returns the overarching type of `d`.
  private mutating func realize(initializerDecl d: InitializerDecl.ID) -> AnyType {
    _realize(decl: d, { (this, d) in this._realize(initializerDecl: d) })
  }

  private mutating func _realize(initializerDecl d: InitializerDecl.ID) -> AnyType {
    // Handle memberwise initializers.
    if ast[d].isMemberwise {
      let productTypeDecl = ProductTypeDecl.ID(program.declToScope[d]!)!
      if let lambda = memberwiseInitType(of: productTypeDecl) {
        return ^lambda
      } else {
        return .error
      }
    }

    var inputs = realize(parameters: ast[d].parameters)

    // Initializers are global functions.
    let receiver = realizeSelfTypeExpr(in: program.declToScope[d]!)!.instance
    let receiverParameter = CallableTypeParameter(
      label: "self",
      type: ^ParameterType(.set, receiver))
    inputs.insert(receiverParameter, at: 0)
    return ^LambdaType(environment: .void, inputs: inputs, output: .void)
  }

  /// Returns the overarching type of `d`.
  private mutating func realize(methodDecl d: MethodDecl.ID) -> AnyType {
    _realize(decl: d, { (this, d) in this._realize(methodDecl: d) })
  }

  private mutating func _realize(methodDecl d: MethodDecl.ID) -> AnyType {
    let inputs = realize(parameters: ast[d].parameters)

    // Realize the method's receiver.
    let receiver = realizeSelfTypeExpr(in: program.declToScope[d]!)!.instance

    // Realize the output type.
    let outputType: AnyType
    if let o = ast[d].output {
      // Use the explicit return annotation.
      guard let type = realize(o, in: AnyScopeID(d))?.instance else { return .error }
      outputType = type
    } else {
      // Default to `Void`.
      outputType = .void
    }

    let m = MethodType(
      capabilities: .init(ast[ast[d].impls].map(\.introducer.value)),
      receiver: receiver,
      inputs: inputs,
      output: outputType)

    for v in ast[d].impls {
      let t = variantType(
        in: m, for: ast[v].introducer.value, reportingDiagnosticsAt: ast[v].introducer.site)
      declTypes[v] = t.map(AnyType.init(_:)) ?? .error
      declRequests[v] = .typeRealizationCompleted
    }

    return ^m
  }

  /// Returns the overarching type of `d`.
  private mutating func realize(methodImpl d: MethodImpl.ID) -> AnyType {
    // `declTypes[d]` is set by the realization of the containing method declaration.
    _realize(decl: d) { (this, d) in
      _ = this.realize(methodDecl: NodeID(this.program.declToScope[d]!)!)
      return this.declTypes[d] ?? .error
    }
  }

  /// Returns the overarching type of `d`.
  private mutating func realize(moduleDecl d: ModuleDecl.ID) -> AnyType {
    _realize(decl: d, { (this, d) in ^ModuleType(d, ast: this.ast) })
  }

  /// Returns the realized types of `parameters`, which are the parameters of an initializer,
  /// method, or subscript declaration.
  private mutating func realize(parameters: [ParameterDecl.ID]) -> [CallableTypeParameter] {
    var result: [CallableTypeParameter] = []
    result.reserveCapacity(parameters.count)
    for p in parameters {
      let i = CallableTypeParameter(
        label: ast[p].label?.value,
        type: realize(parameterDecl: p),
        hasDefault: ast[p].defaultValue != nil)
      result.append(i)
    }
    return result
  }

  /// Returns the overarching type of `d`.
  private mutating func realize(namespaceDecl d: NamespaceDecl.ID) -> AnyType {
    _realize(decl: d) { (this, d) in ^NamespaceType(d, ast: this.ast) }
  }

  /// Returns the overarching type of `d`.
  ///
  /// - Requires: `d` has a type annotation.
  private mutating func realize(parameterDecl d: ParameterDecl.ID) -> AnyType {
    _realize(decl: d) { (this, d) in
      let a = this.ast[d].annotation ?? preconditionFailure("no type annotation")
      let s = this.program.declToScope[d]!
      guard let parameterType = this.realize(parameter: a, in: s)?.instance else {
        return .error
      }

      // The annotation may not omit generic arguments.
      if parameterType[.hasVariable] {
        this.diagnostics.insert(.error(notEnoughContextToInferArgumentsAt: this.ast[a].site))
        return .error
      }
      return parameterType
    }
  }

  /// Returns the overarching type of `d`.
  private mutating func realize(productTypeDecl d: ProductTypeDecl.ID) -> AnyType {
    _realize(decl: d) { (this, d) in ^MetatypeType(of: ProductType(d, ast: this.ast)) }
  }

  /// Returns the overarching type of `d`.
  private mutating func realize(subscriptDecl d: SubscriptDecl.ID) -> AnyType {
    _realize(decl: d, { (this, d) in this._realize(subscriptDecl: d) })
  }

  private mutating func _realize(subscriptDecl d: SubscriptDecl.ID) -> AnyType {
    let inputs = ast[d].parameters.map({ realize(parameters: $0) }) ?? []

    // Collect captures.
    var explicitCaptureNames: Set<Name> = []
    guard
      let explicitCaptureTypes = realize(
        explicitCaptures: ast[d].explicitCaptures,
        collectingNamesIn: &explicitCaptureNames)
    else { return .error }

    let implicitCaptures: [ImplicitCapture] =
      program.isLocal(d)
      ? realize(implicitCapturesIn: d, ignoring: explicitCaptureNames)
      : []
    self.implicitCaptures[d] = implicitCaptures

    // Build the subscript's environment.
    let environment: TupleType
    if program.isNonStaticMember(d) {
      let receiver = realizeSelfTypeExpr(in: program.declToScope[d]!)!.instance
      environment = TupleType([.init(label: "self", type: ^RemoteType(.yielded, receiver))])
    } else {
      environment = TupleType(
        explicitCaptureTypes.map({ (t) in TupleType.Element(label: nil, type: t) })
          + implicitCaptures.map({ (c) in TupleType.Element(label: nil, type: ^c.type) }))
    }

    // Realize the ouput type.
    guard let output = realize(ast[d].output, in: AnyScopeID(d))?.instance else {
      return .error
    }

    // Create a subscript type.
    let m = SubscriptType(
      isProperty: ast[d].parameters == nil,
      capabilities: .init(ast[ast[d].impls].map(\.introducer.value)),
      environment: ^environment,
      inputs: inputs,
      output: output)

    for v in ast[d].impls {
      let t = variantType(
        in: m, for: ast[v].introducer.value, reportingDiagnosticsAt: ast[v].introducer.site)
      declTypes[v] = t.map(AnyType.init(_:)) ?? .error
      declRequests[v] = .typeRealizationCompleted
    }

    return ^m
  }

  /// Returns the overarching type of `d`.
  private mutating func realize(traitDecl d: TraitDecl.ID) -> AnyType {
    _realize(decl: d) { (this, d) in ^MetatypeType(of: TraitType(d, ast: this.ast)) }
  }

  /// Returns the overarching type of `d`.
  private mutating func realize(typeAliasDecl d: TypeAliasDecl.ID) -> AnyType {
    _realize(decl: d, { (this, id) in this._realize(typeAliasDecl: d) })
  }

  private mutating func _realize(typeAliasDecl d: TypeAliasDecl.ID) -> AnyType {
    guard let resolved = realize(ast[d].aliasedType, in: AnyScopeID(d))?.instance else {
      return .error
    }

    let instance = TypeAliasType(aliasing: resolved, declaredBy: NodeID(d)!, in: ast)
    return ^MetatypeType(of: instance)
  }

  /// Returns the overarching type of `d`.
  private mutating func realize<T: TypeExtendingDecl>(typeExtendingDecl d: T.ID) -> AnyType {
    return _realize(decl: d) { (this, d) in
      let t = this.realize(this.ast[d].subject, in: this.program.declToScope[d]!)
      return t.map(AnyType.init(_:)) ?? .error
    }
  }

  /// Returns the overarching type of `d`.
  private mutating func realize(varDecl d: VarDecl.ID) -> AnyType {
    // `declTypes[d]` is set by the realization of the containing binding declaration.
    return _realize(decl: d) { (this, d) in
      _ = this.realize(bindingDecl: this.program.varToBinding[d]!)
      return this.declTypes[d] ?? .error
    }
  }

  /// Realizes the explicit captures in `list`, writing the captured names in `explicitNames`, and
  /// returns their types if they are semantically well-typed. Otherwise, returns `nil`.
  private mutating func realize(
    explicitCaptures list: [BindingDecl.ID],
    collectingNamesIn explictNames: inout Set<Name>
  ) -> [AnyType]? {
    var explictNames: Set<Name> = []
    var captures: [AnyType] = []
    var success = true

    // Process explicit captures.
    for i in list {
      // Collect the names of the capture.
      for (_, namePattern) in ast.names(in: ast[i].pattern) {
        let varDecl = ast[namePattern].decl
        if !explictNames.insert(Name(stem: ast[varDecl].baseName)).inserted {
          diagnostics.insert(
            .error(duplicateCaptureNamed: ast[varDecl].baseName, at: ast[varDecl].site))
          success = false
        }
      }

      // Realize the type of the capture.
      let type = realize(bindingDecl: i)
      if type.isError {
        success = false
      } else {
        switch ast[ast[i].pattern].introducer.value {
        case .let:
          captures.append(^RemoteType(.let, type))
        case .inout:
          captures.append(^RemoteType(.inout, type))
        case .sinklet, .var:
          captures.append(type)
        }
      }
    }

    return success ? captures : nil
  }

  /// Realizes the implicit captures found in the body of `decl` and returns their types and
  /// declarations if they are well-typed. Otherwise, returns `nil`.
  private mutating func realize<T: Decl & LexicalScope>(
    implicitCapturesIn decl: T.ID,
    ignoring explictNames: Set<Name>
  ) -> [ImplicitCapture] {
    var captures: OrderedDictionary<Name, ImplicitCapture> = [:]
    for u in uses(in: AnyDeclID(decl)) {
      var n = ast[u.name].name.value
      if explictNames.contains(n) { continue }

      let candidates = lookup(unqualified: n.stem, in: program.exprToScope[u.name]!)
        .filter({ isCaptured(referenceTo: $0, occuringIn: decl) })
      if candidates.isEmpty { continue }

      guard var c = candidates.uniqueElement else {
        // Ambiguous capture.
        fatalError("not implemented")
      }

      if program.isMember(c) {
        n = .init(stem: "self")
        c = lookup(unqualified: "self", in: AnyScopeID(decl)).uniqueElement!
      }

      modify(&captures[n]) { (x) -> Void in
        let a: AccessEffect = u.isMutable ? .inout : .let
        if let existing = x {
          if (existing.type.access == .let) && (a == .inout) {
            x = existing.mutable()
          }
        } else {
          x = .init(name: n, type: .init(a, realize(decl: c).skolemized), decl: c)
        }
      }
    }
    return Array(captures.values)
  }

  /// Returns the names that are used in `n` along with a list of their occurrences and a flag
  /// indicating whether they are used mutably.
  private func uses(in n: AnyDeclID) -> [(name: NameExpr.ID, isMutable: Bool)] {
    var v = CaptureVisitor()
    ast.walk(n, notifying: &v)
    return v.uses
  }

  /// Returns `true` if references to `c` are captured if they occur in `d`.
  private mutating func isCaptured<T: Decl & LexicalScope>(
    referenceTo c: AnyDeclID, occuringIn d: T.ID
  ) -> Bool {
    if program.isContained(program.declToScope[c]!, in: d) { return false }
    if program.isGlobal(c) { return false }
    if program.isMember(c) {
      // Since the use collector doesn't visit type scopes, if `c` is member then we know that `d`
      // is also a member. If `c` and `d` don't belong to the same type, then the capture is an
      // illegal reference to a foreign receiver.
      assert(program.isMember(d))
      if program.innermostType(containing: c) != program.innermostType(containing: AnyDeclID(d)) {
        return false
      }
    }

    // Capture-less functions are not captured.
    if let f = FunctionDecl.ID(c) {
      guard let t = LambdaType(realize(functionDecl: f)) else { return false }
      return !relations.areEquivalent(t.environment, .void)
    }

    return true
  }

  /// Returns the type of `decl` from the cache, or calls `action` to compute it and caches the
  /// result before returning it.
  private mutating func _realize<T: DeclID>(
    decl id: T,
    _ action: (inout Self, T) -> AnyType
  ) -> AnyType {
    // Check if a type realization request has already been received.
    switch declRequests[id] {
    case nil:
      declRequests[id] = .typeRealizationStarted

    case .typeRealizationStarted:
      diagnostics.insert(.error(circularDependencyAt: ast[id].site))
      declRequests[id] = .done
      declTypes[id] = .error
      return declTypes[id]!

    case .typeRealizationCompleted, .typeCheckingStarted, .done:
      return declTypes[id]!
    }

    // Process the request.
    declTypes[id] = action(&self, id)

    // Update the request status.
    declRequests[id] = .typeRealizationCompleted
    return declTypes[id]!
  }

  /// Returns the type of `decl`'s memberwise initializer.
  private mutating func memberwiseInitType(of decl: ProductTypeDecl.ID) -> LambdaType? {
    // Synthesize the receiver type.
    let receiver = realizeSelfTypeExpr(in: decl)!.instance
    var inputs = [CallableTypeParameter(label: "self", type: ^ParameterType(.set, receiver))]

    // List and realize the type of all stored bindings.
    for m in ast[decl].members {
      guard let member = BindingDecl.ID(m) else { continue }
      if realize(bindingDecl: member).isError { return nil }

      for (_, name) in ast.names(in: ast[member].pattern) {
        let d = ast[name].decl
        inputs.append(.init(label: ast[d].baseName, type: ^ParameterType(.sink, declTypes[d]!)))
      }
    }

    return LambdaType(environment: .void, inputs: inputs, output: .void)
  }

  /// Returns the type of variant `v` given a method with type `bundle` or returns `nil` if such
  /// variant is incompatible with `bundle`, reporting diagnostics at `site`.
  ///
  /// - Requires `v` is in `bundle.capabilities`.
  private mutating func variantType(
    in bundle: MethodType, for v: AccessEffect, reportingDiagnosticsAt site: SourceRange
  ) -> LambdaType? {
    precondition(bundle.capabilities.contains(v))

    let environment =
      (v == .sink)
      ? ^TupleType(labelsAndTypes: [("self", bundle.receiver)])
      : ^TupleType(labelsAndTypes: [("self", ^RemoteType(v, bundle.receiver))])

    let output: AnyType
    if (v == .inout) || (v == .set) {
      guard
        let o = TupleType(relations.canonical(bundle.output)),
        o.elements.count == 2,
        o.elements[0].type == relations.canonical(bundle.receiver)
      else {
        let t = TupleType([
          .init(label: "self", type: bundle.receiver),
          .init(label: nil, type: bundle.output),
        ])
        diagnostics.insert(.error(mutatingBundleMustReturn: t, at: site))
        return nil
      }
      output = o.elements[1].type
    } else {
      output = bundle.output
    }

    return LambdaType(
      receiverEffect: v, environment: environment, inputs: bundle.inputs, output: output)
  }

  /// Returns the type of variant `v` given a subscript with type `bundle` or returns `nil` if such
  /// variant is incompatible with `bundle`, reporting diagnostics at `site`.
  ///
  /// - Requires `v` is in `bundle.capabilities`.
  private func variantType(
    in bundle: SubscriptType, for v: AccessEffect, reportingDiagnosticsAt site: SourceRange
  ) -> SubscriptImplType? {
    precondition(bundle.capabilities.contains(v))

    let transformed = bundle.transformParts { (t) in
      switch t.base {
      case let u as ParameterType where u.access == .yielded:
        return .stepInto(^ParameterType(v, u.bareType))
      case let u as RemoteType where u.access == .yielded:
        return .stepInto(^RemoteType(v, u.bareType))
      default:
        return .stepInto(t)
      }
    }

    return SubscriptImplType(
      isProperty: transformed.isProperty,
      receiverEffect: v,
      environment: transformed.environment,
      inputs: transformed.inputs,
      output: transformed.output)
  }

  // MARK: Type role determination

  /// Replaces occurrences of associated types and generic type parameters in `type` by fresh
  /// type variables variables.
  func open(type: AnyType) -> InstantiatedType {
    /// A map from generic parameter type to its opened type.
    var openedParameters: [AnyType: AnyType] = [:]

    func _impl(type: AnyType) -> TypeTransformAction {
      switch type.base {
      case is AssociatedTypeType:
        fatalError("not implemented")

      case is GenericTypeParameterType:
        if let opened = openedParameters[type] {
          // The parameter was already opened.
          return .stepOver(opened)
        } else {
          // Open the parameter.
          let opened = ^TypeVariable()
          openedParameters[type] = opened

          // TODO: Collect constraints

          return .stepOver(opened)
        }

      default:
        // Nothing to do if `type` isn't parameterized.
        if type[.hasGenericTypeParameter] || type[.hasGenericValueParameter] {
          return .stepInto(type)
        } else {
          return .stepOver(type)
        }
      }
    }

    return InstantiatedType(shape: type.transform(_impl(type:)), constraints: [])
  }

  /// Returns the type declared by `d` bound to open variables for each generic parameter
  /// introduced by `d`.
  ///
  /// - Requires: `d` is a a generic product type or type alias declaration.
  func openForUnification(_ d: AnyDeclID) -> BoundGenericType {
    let parameters: [GenericParameterDecl.ID]
    if let decl = ProductTypeDecl.ID(d) {
      parameters = ast[decl].genericClause!.value.parameters
    } else if let decl = TypeAliasDecl.ID(d) {
      parameters = ast[decl].genericClause!.value.parameters
    } else {
      preconditionFailure()
    }

    let b = MetatypeType(declTypes[d])!.instance
    let a = GenericArguments(
      uniqueKeysWithValues: parameters.map({ (key: $0, value: ^TypeVariable()) }))

    return BoundGenericType(b, arguments: a)
  }

  /// Replaces the generic parameters in `subject` by skolems or fresh variables depending on the
  /// whether their declaration is contained in `scope`.
  func instantiate<S: ScopeID>(
    _ subject: AnyType,
    in scope: S,
    cause: ConstraintOrigin
  ) -> InstantiatedType {
    /// A map from generic parameter type to its opened type.
    var openedParameters: [AnyType: AnyType] = [:]

    func _impl(type: AnyType) -> TypeTransformAction {
      switch type.base {
      case is AssociatedTypeType:
        fatalError("not implemented")

      case let base as GenericTypeParameterType:
        // Identify the generic environment that introduces the parameter.
        let site: AnyScopeID
        if base.decl.kind == TraitDecl.self {
          site = AnyScopeID(base.decl)!
        } else {
          site = program.declToScope[base.decl]!
        }

        if program.isContained(scope, in: site) {
          // Skolemize.
          return .stepOver(^SkolemType(quantifying: type))
        } else if let opened = openedParameters[type] {
          // The parameter was already opened.
          return .stepOver(opened)
        } else {
          // Open the parameter.
          let opened = ^TypeVariable()
          openedParameters[type] = opened

          // TODO: Collect constraints

          return .stepOver(opened)
        }

      default:
        // Nothing to do if `type` isn't parameterized.
        if type[.hasGenericTypeParameter] || type[.hasGenericValueParameter] {
          return .stepInto(type)
        } else {
          return .stepOver(type)
        }
      }
    }

    return InstantiatedType(shape: subject.transform(_impl(type:)), constraints: [])
  }

  // MARK: Utils

  /// Returns whether `decl` is a nominal type declaration.
  mutating func isNominalTypeDecl(_ decl: AnyDeclID) -> Bool {
    switch decl.kind {
    case AssociatedTypeDecl.self, ProductTypeDecl.self, TypeAliasDecl.self:
      return true

    case GenericParameterDecl.self:
      return realize(genericParameterDecl: NodeID(decl)!).base is MetatypeType

    default:
      return false
    }
  }

  /// Returns `d` if it has name `n`, otherwise the implementation of `d` with name `n` or `nil`
  /// if no such implementation exists.
  ///
  /// - Requires: The base name of `d` is equal to `n.stem`
  mutating func decl(in d: AnyDeclID, named n: Name) -> AnyDeclID? {
    if !n.labels.isEmpty && (n.labels != labels(d)) {
      return nil
    }

    if let x = n.notation, x != operatorNotation(d) {
      return nil
    }

    // If the looked up name has an introducer, return the corresponding implementation.
    if let introducer = n.introducer {
      guard let m = ast[MethodDecl.ID(d)] else { return nil }
      return m.impls.first(where: { (i) in
        ast[i].introducer.value == introducer
      }).map(AnyDeclID.init(_:))
    }

    return d
  }

  /// Returns the labels of `d`s name.
  ///
  /// Only function, method, or subscript declarations may have labels. This method returns `[]`
  /// for any other declaration.
  func labels(_ d: AnyDeclID) -> [String?] {
    switch d.kind {
    case FunctionDecl.self:
      return ast[ast[FunctionDecl.ID(d)!].parameters].map(\.label?.value)
    case InitializerDecl.self:
      return labels(InitializerDecl.ID(d)!)
    case MethodDecl.self:
      return ast[ast[MethodDecl.ID(d)!].parameters].map(\.label?.value)
    case SubscriptDecl.self:
      return ast[ast[SubscriptDecl.ID(d)!].parameters ?? []].map(\.label?.value)
    default:
      return []
    }
  }

  /// Returns the labels of `d`s name.
  func labels(_ d: InitializerDecl.ID) -> [String?] {
    if let t = LambdaType(declTypes[d]) {
      return Array(t.labels)
    } else if !ast[d].isMemberwise {
      return ["self"] + ast[ast[d].parameters].map(\.label?.value)
    } else {
      let p = ProductTypeDecl.ID(program.declToScope[d]!)!
      return ast[p].members.reduce(into: ["self"]) { (l, m) in
        guard let b = BindingDecl.ID(m) else { return }
        l.append(
          contentsOf: ast.names(in: ast[b].pattern).map({ ast[ast[$0.pattern].decl].baseName }))
      }
    }
  }

  /// Returns the operator notation of `d`'s name, if any.
  private func operatorNotation(_ d: AnyDeclID) -> OperatorNotation? {
    switch d.kind {
    case FunctionDecl.self:
      return ast[FunctionDecl.ID(d)!].notation?.value

    case MethodDecl.self:
      return ast[MethodDecl.ID(d)!].notation?.value

    default:
      return nil
    }
  }

}

/// The state of the visitor collecting captures.
private struct CaptureVisitor: ASTWalkObserver {

  /// A map from name to its uses and known mutability.
  private(set) var uses: [(name: NameExpr.ID, isMutable: Bool)] = []

  /// Records a use of `n` that is known mutable iff `isMutable` is `true`.
  private mutating func recordOccurence(_ n: NameExpr.ID, mutable isMutable: Bool) {
    uses.append((n, isMutable))
  }

  /// Returns the name at the root of the given `lvalue`.
  private func root(_ lvalue: AnyExprID, in ast: AST) -> NameExpr.ID? {
    switch lvalue.kind {
    case NameExpr.self:
      return NameExpr.ID(lvalue)!
    case SubscriptCallExpr.self:
      return root(ast[SubscriptCallExpr.ID(lvalue)!].callee, in: ast)
    default:
      return nil
    }
  }

  mutating func willEnter(_ n: AnyNodeID, in ast: AST) -> Bool {
    if let e = InoutExpr.ID(n) {
      return visit(inoutExpr: e, in: ast)
    }
    if let e = NameExpr.ID(n) {
      return visit(nameExpr: e, in: ast)
    }
    return !(n.kind.value is TypeScope)
  }

  private mutating func visit(inoutExpr e: InoutExpr.ID, in ast: AST) -> Bool {
    if let x = root(ast[e].subject, in: ast) {
      uses.append((x, true))
      return false
    } else {
      return true
    }
  }

  private mutating func visit(nameExpr e: NameExpr.ID, in ast: AST) -> Bool {
    if ast[e].domain == .none {
      uses.append((e, false))
      return false
    } else {
      return true
    }
  }

}

extension TypeChecker.DeclSet {

  /// Inserts `newMatch` in `self` and returns `nil` if `newMatch` is overloadable. Otherwise,
  /// returns `self` if it's not empty or a singleton containing `newMatch` if it is.
  fileprivate mutating func inserting<T: DeclID>(_ newMatch: T) -> Self? {
    if !newMatch.isOverloadable {
      return isEmpty ? [AnyDeclID(newMatch)] : self
    } else {
      insert(AnyDeclID(newMatch))
      return nil
    }
  }

}
