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

  /// A map from generic declarations to their environment.
  private(set) var environments = DeclProperty<GenericEnvironment>()

  /// A map from module to its synthesized declarations.
  private(set) var synthesizedDecls: [ModuleDecl.ID: [SynthesizedFunctionDecl]] = [:]

  /// A map from name expression to its referred declaration.
  private(set) var referredDecls: BindingMap = [:]

  /// A map from sequence expressions to their evaluation order.
  private(set) var foldedSequenceExprs: [SequenceExpr.ID: FoldedSequenceExpr] = [:]

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

  /// Reports the given diagnostics.
  mutating func report<B: Sequence<Diagnostic>>(_ batch: B) {
    diagnostics.formUnion(batch)
  }

  // MARK: Type system

  /// Returns `generic` with occurrences of parameters keying `subtitutions` replaced by their
  /// corresponding value, performing necessary name lookups from `useScope`.
  ///
  /// This method has no effect if `substitutions` is empty.
  private mutating func specialized(
    _ generic: AnyType, applying substitutions: GenericArguments, in useScope: AnyScopeID
  ) -> AnyType {
    return substitutions.isEmpty ? generic : generic.transform(mutating: &self, transform)

    func transform(mutating me: inout Self, _ t: AnyType) -> TypeTransformAction {
      switch t.base {
      case let u as AssociatedTypeType:
        return .stepOver(transform(mutating: &me, u))
      case let u as BoundGenericType:
        return .stepOver(transform(mutating: &me, u))
      case let u as GenericTypeParameterType:
        return .stepOver(transform(mutating: &me, u))
      case let u as ProductType:
        return .stepOver(transform(mutating: &me, u, declaredBy: u.decl))
      case let u as TypeAliasType:
        return .stepOver(transform(mutating: &me, u, declaredBy: u.decl))
      default:
        return .stepInto(t)
      }
    }

    func transform(mutating me: inout Self, _ t: AssociatedTypeType) -> AnyType {
      let d = t.domain.transform(mutating: &me, transform)
      var candidates = me.lookup(me.ast[t.decl].baseName, memberOf: d, exposedTo: useScope)

      // Ignore associated type declaration unless they define a default value that isn't
      // overridden in the conforming type.
      if let i = candidates.firstIndex(where: { $0.kind == AssociatedTypeDecl.self }) {
        if candidates.count > 1 {
          candidates.remove(at: i)
        } else if let v = me.ast[AssociatedTypeDecl.ID(candidates[i])!].defaultValue {
          return me.realize(v)?.instance ?? .error
        }
      }

      if let selected = candidates.uniqueElement {
        return MetatypeType(me.realize(decl: selected))?.instance ?? .error
      } else {
        return .error
      }
    }

    func transform(mutating me: inout Self, _ t: BoundGenericType) -> AnyType {
      ^t.transformArguments(mutating: &me) { (me, v) in
        let w = (v as? AnyType) ?? fatalError("not implemented")
        return w.transform(mutating: &me, transform)
      }
    }

    func transform(mutating me: inout Self, _ t: GenericTypeParameterType) -> AnyType {
      if let v = substitutions[t.decl] {
        return (v as? AnyType) ?? preconditionFailure("expected type")
      } else {
        return ^t
      }
    }

    /// If `t` is an unparameterized generic type, returns its specialization taking the arguments
    /// in `substitutions` corresponding to the parameters introduced by `d`. Otherwise, returns
    /// `t` unchanged.
    ///
    /// - Requires: `t` is not a trait.
    func transform<T: TypeProtocol, D: GenericScope>(
      mutating me: inout Self, _ t: T, declaredBy d: D.ID
    ) -> AnyType {
      assert(!(t is TraitType))
      let parameters = me.ast[d].genericParameters
      if parameters.isEmpty {
        return ^t
      }

      var arguments: GenericArguments = [:]
      for p in parameters {
        arguments[p] = substitutions[p] ?? ^TypeVariable()
      }
      return ^BoundGenericType(t, arguments: arguments)
    }
  }

  /// Returns `s` extended with traits refined by the elements of `s` in `useScope`.
  private mutating func derivedTraits(
    of s: Set<TraitType>, in useScope: AnyScopeID
  ) -> Set<TraitType> {
    s.reduce(into: Set<TraitType>()) { (r, t) in
      r.formUnion(conformedTraits(of: ^t, in: useScope))
    }
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
      if let d = TraitDecl.ID(program[t.decl].scope) {
        return conformedTraits(of: ^TraitType(d, ast: ast), in: useScope)
      }

      // Conformances of other generic parameters are stored in generic environments.
      for s in program.scopes(from: useScope) where s.kind.value is GenericScope.Type {
        let e = environment(of: s)
        result.formUnion(e.conformedTraits(of: type))
      }

    case let t as BoundGenericType:
      result.formUnion(conformedTraits(of: t.base, in: useScope))

    case let t as ProductType:
      let s = program[t.decl].scope
      for u in realize(conformances: ast[t.decl].conformances) {
        result.formUnion(conformedTraits(of: ^u, in: s))
      }

    case let t as TraitType:
      result.insert(t)

      var work = realize(conformances: ast[t.decl].refinements)
      while let base = work.popFirst() {
        if base == t {
          diagnostics.insert(.error(circularRefinementAt: ast[t.decl].identifier.site))
        } else if result.insert(base).inserted {
          let newTraits = realize(conformances: ast[base.decl].refinements)
          work.formUnion(newTraits)
        }
      }

      // Traits can't be refined in extensions; we're done.
      return result

    default:
      break
    }

    // Collect traits declared in conformance declarations.
    for i in extendingDecls(of: type, exposedTo: useScope).filter(ConformanceDecl.self) {
      for t in realize(conformances: ast[i].conformances) {
        result.formUnion(conformedTraits(of: ^t, in: useScope))
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

  /// The bindings whose initializers are being currently visited.
  private var bindingsUnderChecking: DeclSet = []

  /// Sets the inferred type of `d` to `type`.
  ///
  /// - Requires: `d` has been assigned to a type yet.
  mutating func setInferredType(_ type: AnyType, for d: VarDecl.ID) {
    precondition(declTypes[d] == nil)
    declTypes[d] = type
  }

  /// Type checks the specified module, accumulating diagnostics in `self.diagnostics`.
  ///
  /// This method is idempotent. After the first call for a module `m`, `self.declType[m]` is
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
    for d in program[u].decls.lazy.compactMap(ImportDecl.ID.init(_:)) {
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
      check(method: NodeID(program[d].scope)!)
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
  /// If `asOptional` is `true`, `d` is checked as an optional binding declaration, meaning that
  /// its annotation (if any) is treated as a lower bound rather than an upper bound on tne type
  /// of its initializer.
  ///
  /// - Note: Method is internal because it may be called during constraint generation.
  @discardableResult
  mutating func check(binding d: BindingDecl.ID, asCondition: Bool = false) -> AnyType {
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

    // Determine the type of the declaration's pattern.
    let pattern = inferredType(of: AnyPatternID(ast[d].pattern), shapedBy: nil)
    assert(pattern.facts.inferredTypes.isEmpty, "expression in binding pattern")
    if pattern.type[.hasError] {
      return complete(.error)
    }

    // Type check the initializer, if any.
    guard let initializer = ast[d].initializer else {
      assert(program[d].pattern.annotation != nil, "expected type annotation")
      return complete(pattern.type)
    }

    let initializerType = exprTypes[initializer].setIfNil(^TypeVariable())
    var constraints = pattern.facts.constraints

    // If `d` has no annotation, its type is inferred as that of its initializer. Otherwise, the
    // type of the initializer must be subtype of the pattern unless `d` is used as a condition.
    // In that case, it must be supertype of the pattern.
    if program[d].pattern.annotation == nil {
      let o = ConstraintOrigin(.initializationWithPattern, at: ast[initializer].site)
      constraints.append(EqualityConstraint(initializerType, pattern.type, origin: o))
    } else if !asCondition {
      let o = ConstraintOrigin(.initializationWithHint, at: ast[initializer].site)
      constraints.append(SubtypingConstraint(initializerType, pattern.type, origin: o))
    } else {
      let o = ConstraintOrigin(.optionalBinding, at: ast[initializer].site)
      constraints.append(SubtypingConstraint(pattern.type, initializerType, origin: o))
    }

    // Infer the type of the initializer
    let names = ast.names(in: ast[d].pattern).map({ AnyDeclID(ast[$0.pattern].decl) })
    bindingsUnderChecking.formUnion(names)
    let inference = solutionTyping(
      initializer,
      shapedBy: pattern.type,
      initialConstraints: constraints)
    bindingsUnderChecking.subtract(names)

    // TODO: Complete underspecified generic signatures
    // TODO: Ensure that the initializer is either movable or the result of a constructor call

    let result = complete(inference.solution.typeAssumptions.reify(pattern.type))

    // Run deferred queries.
    let s = pattern.deferred.reduce(true, { $1(&self, inference.solution) && $0 })
    assert(s || diagnostics.containsError)
    return result
  }

  private mutating func check(conformance d: ConformanceDecl.ID) {
    _check(decl: d, { (this, d) in this._check(conformance: d) })
  }

  private mutating func _check(conformance d: ConformanceDecl.ID) {
    // Nothing to do if type realization failed.
    guard declTypes[d]! != .error else { return }

    // Type check the generic constraints.
    _ = environment(ofTypeExtendingDecl: d)

    check(conformanceList: ast[d].conformances, partOf: d)
    check(all: ast[d].members)
  }

  private mutating func check(extension d: ExtensionDecl.ID) {
    _check(decl: d, { (this, d) in this._check(extension: d) })
  }

  private mutating func _check(extension d: ExtensionDecl.ID) {
    // Nothing to do if type realization failed.
    guard declTypes[d]! != .error else { return }

    // Type check the generic constraints.
    _ = environment(ofTypeExtendingDecl: d)

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
      _ = solutionTyping(body, shapedBy: t, initialConstraints: [c])

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
        between: [
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
      _ = checkedType(of: e, subtypeOf: LambdaType(declTypes[d])!.output)
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
  private mutating func check(parameter d: ParameterDecl.ID, siblingNames: inout Set<String>) {
    // Check for duplicate parameter names.
    if !siblingNames.insert(ast[d].baseName).inserted {
      diagnostics.insert(.error(duplicateParameterNamed: ast[d].baseName, at: ast[d].site))
    }

    // Type check the default value, if any.
    if let defaultValue = ast[d].defaultValue {
      let parameterType = ParameterType(declTypes[d]!)!
      let defaultValueType = exprTypes[defaultValue].setIfNil(^TypeVariable())

      _ = solutionTyping(
        defaultValue, shapedBy: parameterType.bareType,
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
    let source = TranslationUnit.ID(program[d].scope)!
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
      _ = checkedType(of: e, subtypeOf: outputType)
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
    let t = declTypes[d]!
    guard !t.isError else { return }

    _ = environment(ofTrait: d)
    check(all: ast[d].members)
    check(all: extendingDecls(of: t, exposedTo: program[d].scope))

    // TODO: Check refinements
  }

  private mutating func check(typeAlias d: TypeAliasDecl.ID) {
    _check(decl: d, { (this, id) in this._check(typeAlias: id) })
  }

  private mutating func _check(typeAlias d: TypeAliasDecl.ID) {
    guard let t = MetatypeType(declTypes[d]!)?.instance else { return }
    _ = environment(of: d)
    check(all: extendingDecls(of: t, exposedTo: program[d].scope))

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
        // checking, we should re-check the status of the request.
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
    let receiver = realizeReceiver(in: d)!.instance
    for e in traits {
      guard let rhs = realize(name: e)?.instance else {
        continue
      }
      guard rhs.base is TraitType else {
        diagnostics.insert(.error(conformanceToNonTraitType: rhs, at: ast[e].site))
        continue
      }

      for t in conformedTraits(of: rhs, in: program[d].scope) {
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

    // Check the trait's requirements.
    var implementations = Conformance.ImplementationMap()
    var notes: DiagnosticSet = []
    for requirement in ast[trait.decl].members {
      checkStatisifed(requirement)
    }

    if !notes.isEmpty {
      diagnostics.insert(.error(model, doesNotConformTo: trait, at: declSite, because: notes))
      return nil
    }

    // Conformances at file scope are exposed in the whole module. Other conformances are exposed
    // in their containing scope.
    let expositionScope = read(program[source].scope) { (s) in
      (s.kind == TranslationUnit.self) ? AnyScopeID(program.module(containing: s)) : s
    }

    // FIXME: Use bound generic parameters as conditions
    let m = BoundGenericType(model).map(\.base) ?? model
    return Conformance(
      model: m, concept: trait, arguments: [:], conditions: [],
      scope: expositionScope, implementations: implementations, isStructural: false,
      site: declSite)

    /// Checks if `requirement` is satisfied by `model`, extending `implementations` if it is or
    /// reporting a diagnostic in `notes` otherwise.
    func checkStatisifed(_ requirement: AnyDeclID) {
      switch requirement.kind {
      case GenericParameterDecl.self:
        assert(requirement == ast[trait.decl].selfParameterDecl, "unexpected declaration")

      case AssociatedTypeDecl.self:
        // TODO: Implement me.
        break

      case AssociatedValueDecl.self:
        // TODO: Implement me.
        break

      case FunctionDecl.self:
        let m = FunctionDecl.ID(requirement)!
        checkSatisfied(callable: m, named: Name(of: m, in: ast)!)

      case InitializerDecl.self:
        let m = InitializerDecl.ID(requirement)!
        checkSatisfied(callable: m, named: Name(of: m, in: ast))

      case MethodDecl.self:
        let r = MethodDecl.ID(requirement)!
        let n = Name(of: r, in: ast)
        ast[r].impls.forEach { (i) in
          checkSatisfied(callable: i, named: n.appending(ast[i].introducer.value)!)
        }

      case SubscriptDecl.self:
        // TODO: Implement me.
        break

      default:
        unreachable()
      }
    }

    /// Checks if `requirement` is satisfied by `model`, extending `implementations` if it is or
    /// reporting a diagnostic in `notes` otherwise.
    func checkSatisfied<D: Decl>(callable requirement: D.ID, named requiredName: Name) {
      guard let requiredType = LambdaType(candidateType(requirement)) else {
        return
      }

      if let c = implementation(
        of: requiredName, in: model,
        withCallableType: requiredType, specializedWith: specializations,
        exposedTo: useScope)
      {
        implementations[requirement] = .concrete(c)
        return
      }

      if let k = ast.synthesizedImplementation(of: requirement, definedBy: trait) {
        let i = SynthesizedFunctionDecl(k, typed: requiredType, in: useScope)
        implementations[requirement] = .synthetic(i)
        synthesizedDecls[program.module(containing: source), default: []].append(i)
        return
      }

      let note = Diagnostic.note(
        trait: trait, requires: requirement.kind, named: requiredName, typed: ^requiredType,
        at: declSite)
      notes.insert(note)
    }

    /// Returns the type of `candidate` viewed as a member of `model` satisfying a requirement in
    /// `concept`, or `nil` if this type could not be realized.
    func candidateType<D: Decl>(_ candidate: D.ID) -> AnyType? {
      let t = realize(decl: candidate)
      if t[.hasError] {
        return nil
      }

      let u = specialized(t, applying: specializations, in: useScope)
      return relations.canonical(u)
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

  /// Type checks `s`.
  private mutating func check<T: StmtID>(stmt s: T) {
    switch s.kind {
    case AssignStmt.self:
      check(assign: NodeID(s)!)
    case BraceStmt.self:
      check(braceStmt: NodeID(s)!)
    case ConditionalStmt.self:
      check(conditional: NodeID(s)!)
    case ExprStmt.self:
      check(exprStmt: NodeID(s)!)
    case DeclStmt.self:
      check(decl: ast[DeclStmt.ID(s)!].decl)
    case DiscardStmt.self:
      _ = checkedType(of: ast[DiscardStmt.ID(s)!].expr)
    case DoWhileStmt.self:
      check(doWhile: NodeID(s)!)
    case ReturnStmt.self:
      check(return: NodeID(s)!)
    case WhileStmt.self:
      check(while: NodeID(s)!)
    case YieldStmt.self:
      check(yield: NodeID(s)!)
    case ForStmt.self, BreakStmt.self, ContinueStmt.self:
      fatalError("not implemented")
    default:
      unexpected(s, in: ast)
    }
  }

  /// Type checks `s`.
  ///
  /// - Note: Method is internal because it may be called during constraint generation.
  mutating func check(braceStmt s: BraceStmt.ID) {
    ast[s].stmts.forEach({ (x) in check(stmt: x) })
  }

  private mutating func check(assign s: AssignStmt.ID) {
    // Target type must be `Movable`.
    guard let targetType = checkedType(of: ast[s].left) else { return }
    let lhsConstraint = ConformanceConstraint(
      targetType, conformsTo: ast.movableTrait,
      origin: ConstraintOrigin(.initializationOrAssignment, at: ast[s].site))

    // Source type must be subtype of the target type.
    let sourceType = exprTypes[ast[s].right].setIfNil(^TypeVariable())
    let rhsConstraint = SubtypingConstraint(
      sourceType, targetType,
      origin: ConstraintOrigin(.initializationOrAssignment, at: ast[s].site))

    // Note: Type information flows strictly from left to right.
    _ = solutionTyping(
      ast[s].right, shapedBy: targetType,
      initialConstraints: [lhsConstraint, rhsConstraint])
  }

  private mutating func check(conditional s: ConditionalStmt.ID) {
    let bool = AnyType(ast.coreType("Bool")!)

    for c in ast[s].condition {
      switch c {
      case .expr(let e):
        _ = checkedType(of: e, subtypeOf: bool)
      case .decl(let d):
        _ = check(binding: d, asCondition: true)
      }
    }

    check(braceStmt: ast[s].success)
    if let b = ast[s].failure {
      check(stmt: b)
    }
  }

  private mutating func check(exprStmt s: ExprStmt.ID) {
    guard let result = checkedType(of: ast[s].expr) else { return }

    // Warn against unused result if the type of the expression is neither `Void` nor `Never`.
    let t = relations.canonical(result)
    if (t != .void) && (t != .never) {
      diagnostics.insert(.warning(unusedResultOfType: result, at: program[s].expr.site))
    }
  }

  private mutating func check(doWhile subject: DoWhileStmt.ID) {
    check(braceStmt: ast[subject].body)

    // Visit the condition of the loop in the scope of the body.
    let boolType = AnyType(ast.coreType("Bool")!)
    check(ast[subject].condition, hasType: boolType, cause: .structural)
  }

  private mutating func check(return s: ReturnStmt.ID) {
    let o = expectedOutputType(in: program[s].scope)!
    if let v = ast[s].value {
      _ = checkedType(of: v, subtypeOf: o)
    } else if !relations.areEquivalent(o, .void) {
      diagnostics.insert(.error(missingReturnValueAt: ast[s].site))
    }
  }

  private mutating func check(while s: WhileStmt.ID) {
    // Visit the condition(s).
    let boolType = AnyType(ast.coreType("Bool")!)
    for item in ast[s].condition {
      switch item {
      case .expr(let e):
        // Condition must be Boolean.
        check(e, hasType: boolType, cause: .structural)
      case .decl(let binding):
        check(binding: binding)
      }
    }

    // Visit the body.
    check(braceStmt: ast[s].body)
  }

  private mutating func check(yield s: YieldStmt.ID) {
    let o = expectedOutputType(in: program[s].scope)!
    _ = checkedType(of: ast[s].value, subtypeOf: o)
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
    for (i, p) in program[e].decl.parameters.enumerated() {
      declTypes[p] = declType.inputs[i].type
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
    case InitializerDecl.self:
      return environment(of: InitializerDecl.ID(scope)!)
    case MethodDecl.self:
      return environment(of: MethodDecl.ID(scope)!)
    case ProductTypeDecl.self:
      return environment(of: ProductTypeDecl.ID(scope)!)
    case SubscriptDecl.self:
      return environment(of: SubscriptDecl.ID(scope)!)
    case TypeAliasDecl.self:
      return environment(of: TypeAliasDecl.ID(scope)!)
    case TraitDecl.self:
      return environment(ofTrait: NodeID(scope)!)
    default:
      unreachable()
    }
  }

  /// Returns the generic environment defined by `d`.
  private mutating func environment<T: GenericDecl>(of d: T.ID) -> GenericEnvironment {
    assert(T.self != TraitDecl.self, "trait environements use a more specialized method")
    if let e = environments[d] {
      return e
    }

    // Nothing to do if the declaration has no generic clause.
    guard let clause = ast[d].genericClause?.value else {
      let e = GenericEnvironment(introducing: [])
      environments[d] = e
      return e
    }

    var result = GenericEnvironment(introducing: clause.parameters)

    // Generate sugared constraints from the parameters' conformance lists.
    for p in clause.parameters {
      let parameter = realize(genericParameterDecl: p)
      if parameter.isError {
        continue
      }

      // TODO: Type check default values.

      // Skip value declarations.
      guard
        let lhs = MetatypeType(parameter)?.instance,
        lhs.base is GenericTypeParameterType
      else { continue }

      // Synthesize the sugared conformance constraint, if any.
      let rhs = ast[p].conformances
      let requiredTraits = realize(conformances: rhs)
      if !requiredTraits.isEmpty {
        let allTraits = derivedTraits(of: requiredTraits, in: AnyScopeID(d)!)
        let s = ast[p].identifier.site
        result.insertConstraint(.init(.conformance(lhs, allTraits), at: s))
      }
    }

    // Generate constraints defined in the where clause.
    if let whereClause = clause.whereClause?.value {
      for expr in whereClause.constraints {
        if let c = eval(constraint: expr) {
          result.insertConstraint(c)
        }
      }
    }

    environments[d] = result
    return result
  }

  /// Returns the generic environment defined by `d`, or `nil` if it is ill-typed.
  private mutating func environment<T: TypeExtendingDecl>(
    ofTypeExtendingDecl d: T.ID
  ) -> GenericEnvironment {
    if let e = environments[d] {
      return e
    }

    var result = GenericEnvironment(introducing: [])
    if let whereClause = ast[d].whereClause?.value {
      for e in whereClause.constraints {
        if let c = eval(constraint: e) {
          result.insertConstraint(c)
        }
      }
    }

    environments[d] = result
    return result
  }

  /// Returns the generic environment defined by `d`, or `nil` if it is ill-typed.
  private mutating func environment(ofTrait d: TraitDecl.ID) -> GenericEnvironment {
    if let e = environments[d] {
      return e
    }

    let receiver = ast[d].selfParameterDecl
    var result = GenericEnvironment(introducing: [receiver])

    // Collect and type check the constraints defined on associated types and values.
    for member in ast[d].members {
      switch member.kind {
      case AssociatedTypeDecl.self:
        insertConsraints(of: NodeID(member)!, in: d, to: &result)
      case AssociatedValueDecl.self:
        insertConstraints(of: NodeID(member)!, in: d, to: &result)
      default:
        continue
      }
    }

    // Synthesize `Self: T`.
    let receiverType = GenericTypeParameterType(receiver, ast: ast)
    let declaredTrait = TraitType(declTypes[d])!
    let c = GenericConstraint(
      .conformance(^receiverType, conformedTraits(of: ^declaredTrait, in: AnyScopeID(d))),
      at: ast[d].identifier.site)
    result.insertConstraint(c)

    environments[d] = result
    return result
  }

  /// Evaluates the valid constraints declared in `associatedType` and adds them to `constraints`.
  private mutating func insertConsraints(
    of associatedType: AssociatedTypeDecl.ID,
    in trait: TraitDecl.ID,
    to env: inout GenericEnvironment
  ) {
    // Realize the LHS of the constraint.
    let lhs = realize(decl: associatedType)
    if lhs.isError { return }

    // Synthesize the sugared conformance constraint, if any.
    let rhs = ast[associatedType].conformances
    let requiredTraits = realize(conformances: rhs)
    if !requiredTraits.isEmpty {
      let allTraits = derivedTraits(of: requiredTraits, in: AnyScopeID(trait))
      let s = ast[associatedType].identifier.site
      env.insertConstraint(.init(.conformance(lhs, allTraits), at: s))
    }

    // Evaluate the constraint expressions of the associated type's where clause.
    if let whereClause = ast[associatedType].whereClause?.value {
      for e in whereClause.constraints {
        if let c = eval(constraint: e) {
          env.insertConstraint(c)
        }
      }
    }
  }

  /// Evaluates the valid constraints declared in `associatedValue` and adds them to `constraints`.
  private mutating func insertConstraints(
    of associatedValue: AssociatedValueDecl.ID,
    in trait: TraitDecl.ID,
    to env: inout GenericEnvironment
  ) {
    // Realize the LHS of the constraint.
    if realize(decl: associatedValue).isError { return }

    // Evaluate the constraint expressions of the associated value's where clause.
    if let whereClause = ast[associatedValue].whereClause?.value {
      for e in whereClause.constraints {
        if let c = eval(constraint: e) {
          env.insertConstraint(c)
        }
      }
    }
  }

  /// Evaluates `e` in `scope` to a generic constraint, or returns `nil` if that failed.
  private mutating func eval(
    constraint e: SourceRepresentable<WhereClause.ConstraintExpr>
  ) -> GenericConstraint? {
    switch e.value {
    case .equality(let l, let r):
      guard let a = realize(name: l)?.instance else { return nil }
      guard let b = realize(r)?.instance else { return nil }

      if !a.isTypeParam && !b.isTypeParam {
        diagnostics.insert(.error(invalidEqualityConstraintBetween: a, and: b, at: e.site))
        return nil
      }

      return .init(.equality(a, b), at: e.site)

    case .conformance(let l, let traits):
      guard let a = realize(name: l)?.instance else { return nil }
      if !a.isTypeParam {
        diagnostics.insert(.error(invalidConformanceConstraintTo: a, at: e.site))
        return nil
      }

      var b: Set<TraitType> = []
      for i in traits {
        guard let type = realize(name: i)?.instance else { return nil }
        if let trait = type.base as? TraitType {
          b.insert(trait)
        } else {
          diagnostics.insert(.error(conformanceToNonTraitType: a, at: e.site))
          return nil
        }
      }

      let allTraits = derivedTraits(of: b, in: program[l].scope)
      return .init(.conformance(a, allTraits), at: e.site)

    case .value(let p):
      // TODO: Symbolic execution
      return .init(.predicate(p), at: e.site)
    }
  }

  // MARK: Type inference

  /// Checks that `e`  has type `t` due to `c`.
  private mutating func check(_ e: AnyExprID, hasType t: AnyType, cause c: ConstraintOrigin.Kind) {
    let u = exprTypes[e].setIfNil(^TypeVariable())
    _ = solutionTyping(
      e, shapedBy: t,
      initialConstraints: [EqualityConstraint(u, t, origin: .init(c, at: ast[e].site))])
  }

  /// Returns the type of `subject` knowing it is shaped by `shape`, or `nil` if no such type could
  /// be deduced.
  ///
  /// - Parameters:
  ///   - subject: The expression whose type should be deduced.
  ///   - shape: The shape of the type `subject` is expected to have given top-bottom information
  ///     flow, or `nil` of such a shape is unknown.
  private mutating func checkedType(
    of subject: AnyExprID, subtypeOf supertype: AnyType? = nil
  ) -> AnyType? {
    var c: [Constraint] = []
    if let t = supertype {
      let u = exprTypes[subject].setIfNil(^TypeVariable())
      c.append(SubtypingConstraint(u, t, origin: .init(.structural, at: ast[subject].site)))
    }

    let i = solutionTyping(subject, shapedBy: supertype, initialConstraints: c)
    return i.succeeded ? exprTypes[subject]! : nil
  }

  /// Returns the best solution satisfying `initialConstraints` and describing the types of
  /// `subject` and its sub-expressions, knowing `subject` is shaped by `shape`.
  ///
  /// - Parameters:
  ///   - subject: The expression whose constituent types should be deduced.
  ///   - shape: The shape of the type `subject` is expected to have given top-bottom information
  ///     flow, or `nil` of such a shape is unknown.
  ///   - initialConstraints: A collection of constraints on constituent types of `subject`.
  mutating func solutionTyping(
    _ subject: AnyExprID, shapedBy shape: AnyType?,
    initialConstraints: [Constraint] = []
  ) -> (succeeded: Bool, solution: Solution) {
    // Determine whether tracing should be enabled.
    let shouldLogTrace: Bool
    if let s = inferenceTracingSite, s.bounds.contains(ast[subject].site.first()) {
      shouldLogTrace = true
      print("Inferring type of '\(program[subject])' at \(ast[subject].site.first())")
      print("---")
    } else {
      shouldLogTrace = false
    }

    // Generate constraints.
    let (_, facts, deferredQueries) = inferredType(of: subject, shapedBy: shape)

    // Bail out if constraint generation failed.
    if facts.foundConflict {
      return (succeeded: false, solution: .init())
    }

    // Solve the constraints.
    var s = ConstraintSystem(
      initialConstraints + facts.constraints,
      bindings: facts.inferredBindings,
      in: program[subject].scope,
      loggingTrace: shouldLogTrace)
    let solution = s.solution(querying: &self)

    if shouldLogTrace {
      print("---")
      print(solution)
    }

    // Apply the solution.
    for (n, r) in solution.bindingAssumptions {
      var s = solution.typeAssumptions.reify(r, withVariables: .kept)

      let t = solution.typeAssumptions.reify(facts.inferredTypes[n]!, withVariables: .kept)
      if t[.hasVariable] || s.arguments.values.contains(where: { $0.isTypeVariable }) {
        report(.error(notEnoughContextToInferArgumentsAt: ast[n].site))
        s = solution.typeAssumptions.reify(s, withVariables: .substitutedByError)
      }

      referredDecls[n] = s
    }

    for (e, t) in facts.inferredTypes {
      exprTypes[e] = solution.typeAssumptions.reify(t, withVariables: .substitutedByError)
    }

    // Run deferred queries.
    let isSound = deferredQueries.reduce(solution.isSound, { (s, q) in q(&self, solution) && s })

    diagnostics.formUnion(solution.diagnostics)
    assert(isSound || diagnostics.containsError, "inference failed without diagnostics")
    return (succeeded: isSound, solution: solution)
  }

  // MARK: Name binding

  /// The result of a name lookup.
  public typealias DeclSet = Set<AnyDeclID>

  /// A lookup table.
  private typealias LookupTable = [String: DeclSet]

  /// A key in a member lookup table.
  private struct MemberLookupKey: Hashable {

    /// The type containing the member being looked up.
    var type: AnyType

    /// The scope in which the member is being used.
    var scope: AnyScopeID

  }

  /// The member lookup tables of the types.
  ///
  /// This property is used to memoize the results of `lookup(_:memberOf:in)`.
  private var memberLookupTables: [MemberLookupKey: LookupTable] = [:]

  /// Resolves the name components of `name` from left to right until either all components have
  /// been resolved or one components requires overload resolution.
  ///
  /// If `name` is prefixed by a non-nominal component, `resolveNonNominalPrefix` is called with a
  /// mutable projection of `self` and the leftmost nominal component `lhs` of `name`, expecting
  /// the type `T` of `lhs`'s domain. If a type is returned, name resolution proceeds, looking for
  /// `lhs` as a member of `T`. Otherwise, the method returns `.canceled(T, u)` is returned, where
  /// `u` is the nominal suffix of `name`, starting from `lhs`.
  mutating func resolve(
    _ name: NameExpr.ID,
    usedAs purpose: NameUse = .unapplied,
    withNonNominalPrefix resolveNonNominalPrefix: (inout Self, NameExpr.ID) -> AnyType?
  ) -> NameResolutionResult {
    var (unresolved, domain) = splitNominalComponents(of: name)

    // Continue iff `name` is prefixed by nominal components only.
    var parent: NameResolutionContext? = nil
    if domain != .none {
      guard let p = resolveNonNominalPrefix(&self, unresolved.last!) else {
        return .canceled(nil, unresolved)
      }

      switch p.base {
      case .error:
        return .failed
      case is TypeVariable:
        return .canceled(p, unresolved)
      default:
        parent = .init(type: p, receiver: .init(domain))
      }
    }

    // Process unresolved components from left to right as long as we don't need contextual
    // information to resolve overload sets.
    var resolved: [NameResolutionResult.ResolvedComponent] = []
    while let component = unresolved.popLast() {
      let candidates = resolve(
        component, in: parent, usedAs: unresolved.isEmpty ? purpose : .unapplied)
      if candidates.isEmpty {
        return .failed
      }

      // Append the resolved component to the nominal prefix.
      resolved.append(.init(component, candidates))

      // Defer resolution of the remaining name components if there are multiple candidates for
      // the current component or if we found a type variable. Otherwise, configure `parent` to
      // resolve the next name component.
      if (candidates.count > 1) || (candidates[0].type.base is TypeVariable) { break }
      let pick = candidates[0]

      // If the candidate is a metatype, perform the next lookup in its instance.
      let receiver = DeclReference.Receiver.explicit(AnyExprID(component))
      let receiverType = MetatypeType(pick.type)?.instance ?? pick.type
      parent = .init(type: receiverType, arguments: pick.reference.arguments, receiver: receiver)
    }

    precondition(!resolved.isEmpty)
    return .done(resolved: resolved, unresolved: unresolved)
  }

  /// Returns `(head, tail)` where `head` contains the nominal components of `name` from right to
  /// left and `tail` is the non-nominal component of `name`, if any.
  ///
  /// Name expressions are rperesented as linked-list, whose elements are the components of a
  /// name in reverse order. This method splits such lists at the first non-nominal component.
  private func splitNominalComponents(of name: NameExpr.ID) -> ([NameExpr.ID], NameExpr.Domain) {
    var suffix = [name]
    while true {
      let d = ast[suffix.last!].domain
      switch d {
      case .none, .implicit, .operand:
        return (suffix, d)
      case .explicit(let e):
        guard let p = NameExpr.ID(e) else { return (suffix, d) }
        suffix.append(p)
      }
    }
  }

  /// Resolves a single name component `n` being used as `purpose` in `context`.
  private mutating func resolve(
    _ n: NameExpr.ID, in context: NameResolutionContext?, usedAs purpose: NameUse
  ) -> [NameResolutionResult.Candidate] {
    // Evaluate the static argument list.
    var arguments: [AnyType] = []
    for a in ast[n].arguments {
      arguments.append(realize(a.value)?.instance ?? .error)
    }

    // Resolve the component.
    let candidates = resolve(
      ast[n].name, parameterizedBy: arguments,
      in: context, exposedTo: program[n].scope, usedAs: purpose)

    // Diagnose undefined symbols.
    if candidates.elements.isEmpty {
      report(.error(undefinedName: ast[n].name.value, in: context?.type, at: ast[n].name.site))
      return []
    }

    // Diagnose symbols with no viable candidates.
    if candidates.viable.isEmpty {
      if let c = candidates.elements.uniqueElement {
        report(c.diagnostics.elements)
      } else {
        report(.error(noViableCandidateToResolve: ast[n].name, notes: []))
      }
      return []
    }

    return candidates.viable.map({ candidates.elements[$0] })
  }

  /// Returns the declarations of `name` exposed to `useScope` and parameterized by `arguments`.
  ///
  /// The declarations are searched with an unqualified lookup unless `context` is set, in which
  /// case they are searched in the declaration space of `context.type`. Generic candidates are
  /// specialized with `arguments` appended to `parent.arguments`.
  mutating func resolve(
    _ name: SourceRepresentable<Name>,
    parameterizedBy arguments: [any CompileTimeValue],
    in context: NameResolutionContext?,
    exposedTo useScope: AnyScopeID,
    usedAs purpose: NameUse
  ) -> NameResolutionResult.CandidateSet {
    // Resolve references to the built-in symbols.
    if context?.type == .builtin(.module) {
      return resolve(builtin: name)
    }

    // Gather declarations qualified by `parent` if it isn't `nil` or unqualified otherwise.
    let matches = lookup(name, memberOf: context?.type, exposedTo: useScope)

    // Resolve intrinsic type aliases if no match was found.
    if matches.isEmpty {
      if context == nil {
        return resolve(intrinsic: name, parameterizedBy: arguments, exposedTo: useScope)
      } else {
        return []
      }
    }

    // Create declaration references to all candidates.
    var candidates: NameResolutionResult.CandidateSet = []
    let parentArguments = context?.arguments ?? [:]
    for m in matches {
      guard var candidateType = resolvedType(of: m) else { continue }
      var log = DiagnosticSet()

      // Keep track of generic arguments that should be captured later on.
      let candidateArguments: GenericArguments
      if let g = BoundGenericType(candidateType) {
        assert(arguments.isEmpty, "generic declaration bound twice")
        candidateArguments = g.arguments
      } else {
        candidateArguments =
          associateGenericParameters(
            genericParameters(introducedBy: m), of: name, to: arguments,
            reportingDiagnosticsTo: &log)
      }

      // If the name resolves to an initializer, determine if it is used as a constructor.
      let isConstructor =
        (m.kind == InitializerDecl.self)
        && ((purpose == .constructor) || (name.value.stem == "new"))
      if isConstructor {
        candidateType = ^LambdaType(constructorFormOf: LambdaType(candidateType)!)
      }

      // If the receiver is an existential, replace its receiver.
      if let t = ExistentialType(context?.type) {
        candidateType = opacize(candidateType, for: t)
      }

      // If the match is introduced in a trait, parameterize its receiver as necessary.
      var allArguments = parentArguments
      if let concept = TraitDecl.ID(program.scopeIntroducing(m)), let model = context?.type {
        allArguments[ast[concept].selfParameterDecl] = model
      }
      allArguments.append(candidateArguments)
      candidateType = specialized(candidateType, applying: allArguments, in: useScope)

      let r = makeReference(
        to: m, parameterizedBy: allArguments, memberOf: context, exposedTo: useScope,
        usedAsConstructor: isConstructor)

      if let sugars = resolve(
        sugared: name,
        memberOf: .init(type: candidateType, arguments: allArguments, receiver: .elided(r)),
        exposedTo: useScope, usedAs: purpose)
      {
        candidates.formUnion(sugars)
        continue
      }

      if (context?.type.base is TraitType) && (m.kind == AssociatedTypeDecl.self) {
        log.insert(.error(invalidUseOfAssociatedType: name.value.stem, at: name.site))
      }

      candidates.insert(
        .init(reference: r, type: candidateType, constraints: [], diagnostics: log))
    }

    return candidates
  }

  /// Returns the declaration of `name` interpreted as a member of the built-in module.
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

  /// Returns the declarations of `name` interpreted as an intrinsic type alias (e.g., `Never` or
  /// `Union<A, B>`) parameterized by `arguments`, or `nil` if an error occured.
  private mutating func resolve(
    intrinsic name: SourceRepresentable<Name>,
    parameterizedBy arguments: [any CompileTimeValue],
    exposedTo useScope: AnyScopeID
  ) -> NameResolutionResult.CandidateSet {
    func nonGeneric(_ t: MetatypeType) -> NameResolutionResult.CandidateSet {
      if arguments.count > 0 {
        report(.error(argumentToNonGenericType: t.instance, at: name.site))
        return [.intrinsic(.error)]
      }
      return [.intrinsic(^t)]
    }

    // TODO: Check labels and notations.
    switch name.value.stem {
    case "Any":
      return nonGeneric(MetatypeType(of: .any))

    case "Never":
      return nonGeneric(MetatypeType(of: .never))

    case "Builtin" where isBuiltinModuleVisible:
      return [.builtinModule]

    case "Union":
      return resolve(union: name, parameterizedBy: arguments)

    case "Self":
      guard let t = realizeReceiver(in: useScope) else {
        report(.error(invalidReferenceToSelfTypeAt: name.site))
        return [.intrinsic(.error)]
      }
      return nonGeneric(t)

    case "Metatype":
      return resolve(metatype: name, parameterizedBy: arguments)

    default:
      return []
    }
  }

  /// Resolves `name` as a reference to a union type parameterized by `arguments`.
  private mutating func resolve(
    union name: SourceRepresentable<Name>,
    parameterizedBy arguments: [any CompileTimeValue]
  ) -> NameResolutionResult.CandidateSet {
    var elements: [AnyType] = []
    for a in arguments {
      guard let t = a as? AnyType else {
        report(.error(valueInUnionTypeAt: name.site))
        return [.intrinsic(.error)]
      }
      elements.append(t)
    }

    switch arguments.count {
    case 0:
      report(.warning(unionTypeWithZeroElementsAt: name.site))
      return [.intrinsic(^MetatypeType(of: .never))]
    case 1:
      report(.error(unionTypeWithOneElementAt: name.site))
      return [.intrinsic(.error)]
    default:
      return [.intrinsic(^MetatypeType(of: UnionType(elements)))]
    }
  }

  /// Resolves `name` as a reference to a metatype parameterized by `arguments`.
  private mutating func resolve(
    metatype name: SourceRepresentable<Name>,
    parameterizedBy arguments: [any CompileTimeValue]
  ) -> NameResolutionResult.CandidateSet {
    if let a = arguments.uniqueElement {
      let instance = (a as? AnyType) ?? fatalError("not implemented")
      return [.intrinsic(^MetatypeType(of: MetatypeType(of: instance)))]
    }

    if arguments.isEmpty {
      return [.intrinsic(^MetatypeType(of: MetatypeType(of: TypeVariable())))]
    }

    report(.error(invalidGenericArgumentCountTo: name, found: arguments.count, expected: 1))
    return [.intrinsic(.error)]
  }

  /// Resolves `name` as a sugared reference to a constructor or nameless subscript declaration or
  /// returns `nil` if `name` isn't a sugar.
  private mutating func resolve(
    sugared name: SourceRepresentable<Name>,
    memberOf parent: NameResolutionContext,
    exposedTo useScope: AnyScopeID,
    usedAs purpose: NameUse
  ) -> NameResolutionResult.CandidateSet? {
    // Nothing to do if `parent.type` is a callable type.
    if parent.type.base is CallableType {
      return nil
    }

    switch purpose {
    case .constructor, .function:
      guard let t = MetatypeType(parent.type)?.instance else { return nil }
      let n = SourceRepresentable(value: Name(stem: "init"), range: name.site)
      let r = resolve(
        n, parameterizedBy: [],
        in: .init(type: t, arguments: parent.arguments, receiver: nil),
        exposedTo: useScope, usedAs: .constructor)
      return r.elements.isEmpty ? nil : r

    case .subscript where !(parent.type.base is MetatypeType):
      let n = SourceRepresentable(value: Name(stem: "[]"), range: name.site)
      let r = resolve(
        n, parameterizedBy: [],
        in: parent,
        exposedTo: useScope, usedAs: .subscript)
      return r.elements.isEmpty ? nil : r

    default:
      return nil
    }
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

  private func opacize(_ member: AnyType, for receiver: ExistentialType) -> AnyType {
    let m = LambdaType(member) ?? fatalError("not implemented")

    var r = m.captures[0].type
    if let s = RemoteType(r) {
      r = ^RemoteType(s.access, ^receiver)
    } else {
      r = ^receiver
    }

    return ^LambdaType(
      receiverEffect: m.receiverEffect,
      environment: ^TupleType(labelsAndTypes: [("self", r)]),
      inputs: m.inputs, output: m.output)
  }

  /// Associates `parameters`, which are introduced by `name`'s declaration, to corresponding
  /// values in `arguments` if the two arrays have the same length. Otherwise, returns `nil`,
  /// reporting diagnostics to `log`.
  private mutating func associateGenericParameters(
    _ parameters: [GenericParameterDecl.ID], of name: SourceRepresentable<Name>,
    to arguments: [any CompileTimeValue],
    reportingDiagnosticsTo log: inout DiagnosticSet
  ) -> GenericArguments {
    var result = GenericArguments()
    for (a, p) in zip(arguments, parameters) {
      result[p] = a
    }
    for p in parameters.dropFirst(arguments.count) {
      result[p] = ^GenericTypeParameterType(p, ast: ast)
    }

    if !arguments.isEmpty && (parameters.count != arguments.count) {
      let (f, e) = (arguments.count, parameters.count)
      log.insert(.error(invalidGenericArgumentCountTo: name, found: f, expected: e))
    }

    return result
  }

  /// Returns the generic parameters introduced by `d`.
  private func genericParameters(introducedBy d: AnyDeclID) -> [GenericParameterDecl.ID] {
    if let g = ast[d] as? GenericScope {
      return (d.kind == TraitDecl.self) ? [] : g.genericParameters
    } else {
      return []
    }
  }

  /// Returns a reference to `d` as a member of `parent`, parameterized by `parametrization`,
  /// exposed to `useScope`, and used as a constructor if `isConstructor` is `true`.
  private func makeReference(
    to d: AnyDeclID,
    parameterizedBy parameterization: GenericArguments,
    memberOf parent: NameResolutionContext?,
    exposedTo useScope: AnyScopeID,
    usedAsConstructor isConstructor: Bool
  ) -> DeclReference {
    if isConstructor {
      return .constructor(InitializerDecl.ID(d)!, parameterization)
    } else if program.isNonStaticMember(d) && !(parent?.type.base is MetatypeType) {
      if let p = parent {
        return .member(d, parameterization, p.receiver!)
      } else {
        let r = program.innermostReceiver(in: useScope)!
        return .member(d, parameterization, .elided(.direct(AnyDeclID(r), [:])))
      }
    } else {
      return .direct(d, parameterization)
    }
  }

  /// Returns the type referred by `e`, which is the interface of an existential or the subject of
  /// an extension, or `nil` if `e` can't be resolved as a nominal type declaration.
  ///
  /// When a type expression `e` denotes the interface of an existential type or the subject of an
  /// extension, generic parameters are interpreted as sugared constraints that would otherwise be
  /// defined in a where clause. For example:
  ///
  ///     conformance Array<Int>: P {}
  ///
  /// Here, the extended type is `Array` and `Int` is viewed as a constraint on `Array`.
  private mutating func resolve(interface e: AnyExprID) -> AnyType? {
    guard let i = realize(e) else { return nil }

    if let t = TraitType(i.instance) {
      return ^t
    }

    // Arguments to bound generic types desugar like where clauses. No constraint is created when
    // an argument refers to its corresponding parameter, unless the `d` is nested in the scope
    // where that parameter is introduced.
    if let b = BoundGenericType(i.instance) {
      for (p, a) in b.arguments {
        assert(GenericTypeParameterType(a as! AnyType)?.decl == p, "not implemented")
      }
      return ^MetatypeType(of: b.base)
    }

    return ^i
  }

  /// Returns the declarations exposing a name with given `stem` to `useScope` without
  /// qualification.
  mutating func lookup(unqualified stem: String, in useScope: AnyScopeID) -> DeclSet {
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
      let newMatches = lookup(stem, inDeclSpaceOf: s, exposedTo: useScope)
        .subtracting(bindingsUnderChecking)
      for d in newMatches {
        if let result = matches.inserting(d) { return result }
      }
    }

    // Handle references to the containing module.
    if ast[containingModule]?.baseName == stem {
      if let result = matches.inserting(containingModule!) { return result }
    }

    // Handle references to imported symbols.
    if let u = containingFile, let fileImports = imports[u] {
      for m in fileImports {
        if ast[m].baseName == stem {
          matches.insert(AnyDeclID(m))
        } else {
          matches.formUnion(names(introducedIn: m)[stem, default: []])
        }
      }
    }

    return matches
  }

  /// Returns the declarations introducing a name with given `stem` in the declaration space of
  /// `lookupContext` and exposed to `useScope`.
  private mutating func lookup(
    _ stem: String,
    inDeclSpaceOf lookupContext: AnyScopeID,
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
      return lookup(stem, inDeclSpaceOf: d, exposedTo: useScope)

    case ExtensionDecl.self:
      let d = ExtensionDecl.ID(lookupContext)!
      return lookup(stem, inDeclSpaceOf: d, exposedTo: useScope)

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

  /// Returns the declarations introducing a name with given `stem` in the declaration space of
  /// `lookupContext` and exposed to `useScope`.
  private mutating func lookup<T: TypeExtendingDecl>(
    _ stem: String,
    inDeclSpaceOf lookupContext: T.ID,
    exposedTo useScope: AnyScopeID
  ) -> DeclSet {
    let matches = names(introducedIn: lookupContext)[stem, default: []]
    if let t = MetatypeType(realize(typeExtendingDecl: lookupContext))?.instance {
      return matches.union(lookup(stem, memberOf: t, exposedTo: useScope))
    } else {
      return matches
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
    case let t as ExistentialType:
      return lookup(stem, memberOf: t, exposedTo: useScope)
    case let t as SkolemType:
      return lookup(stem, memberOf: t, exposedTo: useScope)
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

    matches.formUnion(lookup(stem, inExtensionsOf: domain, exposedTo: useScope))
    return matches
  }

  /// Returns the declarations introducing a name with given `stem` as a member of `domain` and
  /// exposed to `useScope`.
  private mutating func lookup(
    _ stem: String,
    memberOf domain: ExistentialType,
    exposedTo useScope: AnyScopeID
  ) -> DeclSet {
    // Declarations in extensions of existential types shadow their existential APIs.
    let matches = lookup(stem, inExtensionsOf: ^domain, exposedTo: useScope)
    if !matches.isEmpty {
      return matches
    }

    // Search in the existential API.
    switch domain.interface {
    case .traits(let s):
      return s.reduce(into: DeclSet()) { (r, t) in
        r.formUnion(lookup(stem, memberOf: ^t, exposedTo: useScope))
      }

    case .generic(let t):
      return lookup(stem, memberOf: t, exposedTo: useScope)

    case .metatype:
      return []
    }
  }

  /// Returns the declarations introducing a name with given `stem` as a member of `domain` and
  /// exposed to `useScope`.
  private mutating func lookup(
    _ stem: String,
    memberOf domain: SkolemType,
    exposedTo useScope: AnyScopeID
  ) -> DeclSet {
    var matches = DeclSet()
    for t in conformedTraits(of: domain.base, in: useScope) {
      matches.formUnion(lookup(stem, memberOf: ^t, exposedTo: useScope))
    }
    return matches
  }

  /// Returns the declarations introducing a name with given `stem` in extensions of `domain`
  /// exposed to `useScope`.
  private mutating func lookup(
    _ stem: String,
    inExtensionsOf domain: AnyType,
    exposedTo useScope: AnyScopeID
  ) -> DeclSet {
    var matches = DeclSet()

    // Look for members declared in extensions.
    for i in extendingDecls(of: domain, exposedTo: useScope) {
      matches.formUnion(names(introducedIn: AnyScopeID(i)!)[stem, default: []])
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
    for s in program.scopes(from: useScope) {
      switch s.kind {
      case ModuleDecl.self:
        let m = ModuleDecl.ID(s)!
        let symbols = ast.topLevelDecls(m)
        insert(decls: symbols, extending: canonicalSubject, into: &matches)
        root = m

      case TranslationUnit.self:
        continue

      default:
        insert(decls: program[s].decls, extending: canonicalSubject, into: &matches)
      }
    }

    // Nowhere else to look if `useScope` is a module.
    if useScope.kind == ModuleDecl.self { return matches }

    // Look for extension declarations in imported modules.
    let imports = self.imports[program.source(containing: useScope), default: []]
    for m in imports where m != root {
      let symbols = ast.topLevelDecls(m)
      insert(decls: symbols, extending: canonicalSubject, into: &matches)
    }

    return matches
  }

  /// Insert the declarations in `decls` that extend `subject` into `matches`.
  ///
  /// - Requires: `subject` must be canonical.
  private mutating func insert<S: Sequence>(
    decls: S, extending subject: AnyType, into matches: inout [AnyDeclID]
  ) where S.Element == AnyDeclID {
    precondition(subject[.isCanonical])

    for d in decls where (d.kind == ConformanceDecl.self) || (d.kind == ExtensionDecl.self) {
      // Skip extending declarations that are already on the lookup stack.
      // Note: This check assumes this thread has exclusive access to `declRequests`, so that the
      // only way type realization can have started is if we did. Without this assumption we may
      // incorrectly skip an extension that's been type checked in another thread.
      if declRequests[d] == .typeRealizationStarted { continue }

      // Check for matches.
      guard let extendedType = realize(decl: d).base as? MetatypeType else { continue }
      if relations.canonical(extendedType.instance) == subject {
        matches.append(d)
      }
    }
  }

  /// Returns the names in `s`.
  private func names<T: ScopeID>(introducedIn s: T) -> LookupTable {
    if let module = ModuleDecl.ID(s) {
      return ast[module].sources.reduce(into: [:]) { (table, s) in
        table.merge(names(introducedIn: s), uniquingKeysWith: { (l, _) in l })
      }
    }

    let decls = program[AnyScopeID(s)!].decls
    if decls.isEmpty { return [:] }

    var table: LookupTable = [:]
    for d in decls {
      switch d.kind {
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
        let name = (ast[d] as! SingleEntityDecl).baseName
        table[name, default: []].insert(d)

      case BindingDecl.self,
        ConformanceDecl.self,
        ExtensionDecl.self,
        MethodImpl.self,
        OperatorDecl.self,
        SubscriptImpl.self:
        // Note: operator declarations are not considered during standard name lookup.
        break

      case FunctionDecl.self:
        guard let i = ast[FunctionDecl.ID(d)!].identifier?.value else { continue }
        table[i, default: []].insert(d)

      case InitializerDecl.self:
        table["init", default: []].insert(d)
        table["new", default: []].insert(d)

      case MethodDecl.self:
        table[ast[MethodDecl.ID(d)!].identifier.value, default: []].insert(d)

      case SubscriptDecl.self:
        let i = ast[SubscriptDecl.ID(d)!].identifier?.value ?? "[]"
        table[i, default: []].insert(d)

      default:
        unexpected(d, in: ast)
      }
    }

    // Note: Results should be memoized.
    return table
  }

  /// If `s` is contained in a type extending declaration, returns the scope extended by that
  /// declaration. Otherwise, returns `nil`.
  private mutating func bridgedScope<S: ScopeID>(of s: S) -> AnyScopeID? {
    for s in program.scopes(from: s) {
      switch s.kind {
      case ConformanceDecl.self:
        return scopeExtended(by: ConformanceDecl.ID(s)!)
      case ExtensionDecl.self:
        return scopeExtended(by: ExtensionDecl.ID(s)!)
      default:
        continue
      }
    }
    return nil
  }

  /// Returns the scope of the declaration extended by `d`, if any.
  private mutating func scopeExtended<T: TypeExtendingDecl>(by d: T.ID) -> AnyScopeID? {
    let t = realize(typeExtendingDecl: d)

    switch MetatypeType(t)?.instance.base {
    case let u as ProductType:
      return AnyScopeID(u.decl)
    case let u as TypeAliasType:
      return AnyScopeID(u.decl)
    default:
      return nil
    }
  }

  // MARK: Type realization

  /// Realizes and returns the type denoted by `e`.
  mutating func realize(_ e: AnyExprID) -> MetatypeType? {
    switch e.kind {
    case ConformanceLensTypeExpr.self:
      return realize(conformanceLens: .init(e)!)
    case ExistentialTypeExpr.self:
      return realize(existentialType: .init(e)!)
    case LambdaTypeExpr.self:
      return realize(lambda: .init(e)!)
    case NameExpr.self:
      return realize(name: .init(e)!)
    case RemoteTypeExpr.self:
      return realize(remoteType: .init(e)!)
    case TupleTypeExpr.self:
      return realize(tuple: .init(e)!)
    case WildcardExpr.self:
      return MetatypeType(of: TypeVariable())

    case BooleanLiteralExpr.self:
      report(.error(typeExprDenotesValue: e, in: ast))
      return nil

    default:
      unexpected(e, in: ast)
    }
  }

  /// Returns the realized type of the function declaration underlying `expr` requiring that its
  /// parameters have the given `conventions`.
  ///
  /// - Requires: if supplied, `conventions` has as one element per parameter of the declaration
  ///   underlying `expr`.
  mutating func realize(
    underlyingDeclOf e: LambdaExpr.ID, with conventions: [AccessEffect]?
  ) -> AnyType? {
    realize(functionDecl: ast[e].decl, with: conventions)
  }

  /// Returns the expression "`Self`" if occured in `useScope`.
  private mutating func realizeReceiver<T: ScopeID>(in useScope: T) -> MetatypeType? {
    for s in program.scopes(from: useScope) {
      switch s.kind {
      case TraitDecl.self:
        let d = TraitDecl.ID(s)!
        return MetatypeType(of: GenericTypeParameterType(selfParameterOf: d, in: ast))
      case ProductTypeDecl.self:
        return realizeReceiver(of: ProductTypeDecl.ID(s)!)
      case ConformanceDecl.self:
        return realizeReceiver(in: ConformanceDecl.ID(s)!)
      case ExtensionDecl.self:
        return realizeReceiver(in: ExtensionDecl.ID(s)!)
      case TypeAliasDecl.self:
        return realizeReceiver(of: TypeAliasDecl.ID(s)!)
      default:
        continue
      }
    }

    // `scope` isn't contained in a type scope.
    return nil
  }

  /// Returns the expressions "`Self`" if it referred to `d`.
  ///
  /// - Requires: `d` is either a product type or type alias declaration.
  private mutating func realizeReceiver<T: GenericDecl>(of d: T.ID) -> MetatypeType? {
    guard let unparameterized = MetatypeType(realize(decl: d)) else {
      return nil
    }

    // Synthesize arguments to generic parameters if necessary.
    if let parameters = ast[d].genericClause?.value.parameters {
      return realizeGenericReceiver(unparameterized.instance, parameterizedBy: parameters)
    } else {
      return unparameterized
    }
  }

  /// Returns the expressions "`Self`" parameterized by `parameters`.
  private mutating func realizeGenericReceiver(
    _ unparameterized: AnyType, parameterizedBy parameters: [GenericParameterDecl.ID]
  ) -> MetatypeType {
    let arguments = GenericArguments(
      uniqueKeysWithValues: parameters.map({ (p) in
        (key: p, value: ^GenericTypeParameterType(p, ast: ast))
      }))
    return MetatypeType(of: BoundGenericType(unparameterized, arguments: arguments))
  }

  /// Returns the expression "`Self`" if it occured directly in given type extending declaration.
  private mutating func realizeReceiver<T: TypeExtendingDecl>(in scope: T.ID) -> MetatypeType? {
    let t = realize(typeExtendingDecl: scope)

    guard let m = MetatypeType(t) else {
      return t.isError ? nil : MetatypeType(of: t)
    }

    switch m.instance.base {
    case let u as ProductType:
      return realizeReceiver(of: u.decl)
    case let u as TypeAliasType:
      return realizeReceiver(of: u.decl)
    default:
      return m
    }
  }

  private mutating func realize(conformanceLens e: ConformanceLensTypeExpr.ID) -> MetatypeType? {
    /// The lens must be a trait.
    guard let lens = realize(ast[e].lens)?.instance else { return nil }
    guard let lensTrait = lens.base as? TraitType else {
      diagnostics.insert(.error(notATrait: lens, at: program[e].lens.site))
      return nil
    }

    // The subject must conform to the lens.
    guard let subject = realize(ast[e].subject)?.instance else { return nil }
    if !conformedTraits(of: subject, in: program[e].scope).contains(lensTrait) {
      diagnostics.insert(.error(subject, doesNotConformTo: lensTrait, at: program[e].lens.site))
      return nil
    }

    return MetatypeType(of: ConformanceLensType(viewing: subject, through: lensTrait))
  }

  mutating func realize(existentialType e: ExistentialTypeExpr.ID) -> MetatypeType? {
    assert(!ast[e].traits.isEmpty, "existential type with no interface")

    // Realize the interface.
    var interface: [AnyType] = []
    for n in ast[e].traits {
      // Expression must refer to a type or trait.
      guard let t = resolve(interface: AnyExprID(n)) else { return nil }
      switch t.base {
      case let u as MetatypeType:
        interface.append(u.instance)
      case let u as TraitType:
        interface.append(^u)
      default:
        report(.error(invalidExistentialInterface: n, in: ast))
        return nil
      }
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
          report(.error(notATrait: interface[i], at: ast[ast[e].traits[i]].site))
          return nil
        }
      }

      return MetatypeType(of: ExistentialType(traits: traits, constraints: []))
    } else if let t = interface.uniqueElement {
      return MetatypeType(of: ExistentialType(unparameterized: t, constraints: []))
    } else {
      report(.error(tooManyExistentialBoundsAt: ast[ast[e].traits[1]].site))
      return nil
    }
  }

  private mutating func realize(lambda e: LambdaTypeExpr.ID) -> MetatypeType? {
    // Realize the lambda's environment.
    let environment: AnyType
    if let environmentExpr = ast[e].environment {
      guard let ty = realize(environmentExpr) else { return nil }
      environment = ty.instance
    } else {
      environment = .any
    }

    // Realize the lambda's parameters.
    var inputs: [CallableTypeParameter] = []
    inputs.reserveCapacity(ast[e].parameters.count)

    for p in ast[e].parameters {
      guard let ty = realize(parameter: p.type)?.instance else { return nil }
      inputs.append(.init(label: p.label?.value, type: ty))
    }

    // Realize the lambda's output.
    guard let output = realize(ast[e].output)?.instance else { return nil }

    return MetatypeType(
      of: LambdaType(
        receiverEffect: ast[e].receiverEffect?.value ?? .let,
        environment: environment,
        inputs: inputs,
        output: output))
  }

  private mutating func realize(domainOf e: NameExpr.ID) -> AnyType? {
    switch ast[e].domain {
    case .explicit(let d):
      return realize(d).map(\.instance) ?? .error

    case .implicit:
      report(.error(notEnoughContextToResolveMember: ast[e].name))
      return .error

    case .none, .operand:
      unreachable()
    }
  }

  private mutating func realize(name e: NameExpr.ID) -> MetatypeType? {
    let resolution = resolve(e, withNonNominalPrefix: { (this, p) in this.realize(domainOf: p) })
    switch resolution {
    case .done(let prefix, let suffix) where suffix.isEmpty:
      // Nominal type expressions shall not be overloaded.
      guard let candidate = prefix.last!.candidates.uniqueElement else {
        report(.error(ambiguousUse: prefix.last!.component, in: ast))
        return nil
      }

      // Last component must resolve to a type or trait.
      switch candidate.type.base {
      case is MetatypeType, is TraitType:
        break
      case is ErrorType:
        return nil
      default:
        report(.error(typeExprDenotesValue: prefix.last!.component, in: ast))
        return nil
      }

      // Bind each component of the name expression and gather type constraints.
      for p in prefix {
        exprTypes[p.component] = p.candidates[0].type
        referredDecls[p.component] = p.candidates[0].reference
      }

      // FIXME: Avoid wrapping traits in metatypes
      if let t = TraitType(exprTypes[e]) {
        return MetatypeType(of: t)
      } else {
        return MetatypeType(exprTypes[e])!
      }

    case .failed:
      return nil

    case .canceled:
      // Non-nominal prefixes are handled by the closure passed to `resolveNominalPrefix`.
      unreachable()

    default:
      fatalError("not implemented")
    }
  }

  private mutating func realize(parameter e: ParameterTypeExpr.ID) -> MetatypeType? {
    guard let bare = realize(ast[e].bareType)?.instance else {
      return nil
    }
    return MetatypeType(of: ParameterType(ast[e].convention.value, bare))
  }

  private mutating func realize(remoteType e: RemoteTypeExpr.ID) -> MetatypeType? {
    guard let bare = realize(ast[e].operand)?.instance else {
      return nil
    }
    return MetatypeType(of: RemoteType(ast[e].convention.value, bare))
  }

  private mutating func realize(tuple e: TupleTypeExpr.ID) -> MetatypeType? {
    var elements: [TupleType.Element] = []
    elements.reserveCapacity(ast[e].elements.count)

    for m in ast[e].elements {
      guard let ty = realize(m.type)?.instance else { return nil }
      elements.append(.init(label: m.label?.value, type: ty))
    }

    return MetatypeType(of: TupleType(elements))
  }

  /// Realizes and returns the traits denoted by each valid trait expression in `conformances`.
  private mutating func realize(conformances: [NameExpr.ID]) -> Set<TraitType> {
    var result: Set<TraitType> = []
    for e in conformances {
      guard let rhs = realize(name: e)?.instance else { continue }
      if let t = TraitType(rhs) {
        result.insert(t)
      } else {
        diagnostics.insert(.error(conformanceToNonTraitType: rhs, at: ast[e].site))
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
      let traitDecl = TraitDecl.ID(this.program[d].scope)!

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
      let traitDecl = TraitDecl.ID(this.program[d].scope)!

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
      guard let type = realize(o)?.instance else { return .error }
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
      let receiver = realizeReceiver(in: program[d].scope)!.instance
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
      guard let type = realize(name: annotation) else {
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
      let productTypeDecl = ProductTypeDecl.ID(program[d].scope)!
      if let lambda = memberwiseInitType(of: productTypeDecl) {
        return ^lambda
      } else {
        return .error
      }
    }

    var inputs = realize(parameters: ast[d].parameters)

    // Initializers are global functions.
    let receiver = realizeReceiver(in: program[d].scope)!.instance
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
    let receiver = realizeReceiver(in: program[d].scope)!.instance

    // Realize the output type.
    let outputType: AnyType
    if let o = ast[d].output {
      // Use the explicit return annotation.
      guard let type = realize(o)?.instance else { return .error }
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
    // `declType[d]` is set by the realization of the containing method declaration.
    _realize(decl: d) { (this, d) in
      _ = this.realize(methodDecl: NodeID(this.program[d].scope)!)
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
      guard let parameterType = this.realize(parameter: a)?.instance else {
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
      let receiver = realizeReceiver(in: program[d].scope)!.instance
      environment = TupleType([.init(label: "self", type: ^RemoteType(.yielded, receiver))])
    } else {
      environment = TupleType(
        explicitCaptureTypes.map({ (t) in TupleType.Element(label: nil, type: t) })
          + implicitCaptures.map({ (c) in TupleType.Element(label: nil, type: ^c.type) }))
    }

    // Realize the ouput type.
    guard let output = realize(ast[d].output)?.instance else {
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
    _realize(decl: d) { (this, d) in ^TraitType(d, ast: this.ast) }
  }

  /// Returns the overarching type of `d`.
  private mutating func realize(typeAliasDecl d: TypeAliasDecl.ID) -> AnyType {
    _realize(decl: d, { (this, id) in this._realize(typeAliasDecl: d) })
  }

  private mutating func _realize(typeAliasDecl d: TypeAliasDecl.ID) -> AnyType {
    guard let resolved = realize(ast[d].aliasedType)?.instance else {
      return .error
    }

    let instance = TypeAliasType(aliasing: resolved, declaredBy: NodeID(d)!, in: ast)
    return ^MetatypeType(of: instance)
  }

  /// Returns the overarching type of `d`.
  private mutating func realize<T: TypeExtendingDecl>(typeExtendingDecl d: T.ID) -> AnyType {
    _realize(decl: d, { (this, id) in this._realize(typeExtendingDecl: d) })
  }

  private mutating func _realize<T: TypeExtendingDecl>(typeExtendingDecl d: T.ID) -> AnyType {
    guard let i = resolve(interface: ast[d].subject) else {
      return .error
    }

    // FIXME: Temporary hack to let 'extendingDecls()' find us
    if let t = TraitType(i) {
      return ^MetatypeType(of: t)
    }

    if let m = MetatypeType(i), let b = BuiltinType(m.instance) {
      diagnostics.insert(.error(cannotExtend: b, at: ast[ast[d].subject].site))
      return .error
    }

    return i
  }

  /// Returns the overarching type of `d`.
  private mutating func realize(varDecl d: VarDecl.ID) -> AnyType {
    // `declType[d]` is set by the realization of the containing binding declaration.
    return _realize(decl: d) { (this, d) in
      _ = this.realize(bindingDecl: this.program[d].binding)
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
        switch program[i].pattern.introducer.value {
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

      let candidates = lookup(unqualified: n.stem, in: program[u.name].scope)
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
    if program.isContained(program[c].scope, in: d) { return false }
    if program.isGlobal(c) { return false }
    if program.isMember(c) {
      let lhs = realizeReceiver(in: program[c].scope)
      let rhs = realizeReceiver(in: program[d].scope)
      if lhs != rhs {
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
    let receiver = realizeReceiver(in: decl)!.instance
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
  /// a variant is incompatible with `bundle`, reporting diagnostics at `site`.
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
  /// a variant is incompatible with `bundle`, reporting diagnostics at `site`.
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

  /// A context in which a generic parameter can be instantiated.
  private struct InstantiationContext {

    /// The scope in which the parameter is being used.
    let useScope: AnyScopeID

    /// The scope extended by `useScope` if the latter is contained in an extension.
    let extendedScope: AnyScopeID?

    /// Creates an instance for instantiating parameters used in `useScope`, using `checker` to
    /// resolve extensions.
    init(_ useScope: AnyScopeID, mutating checker: inout TypeChecker) {
      self.useScope = AnyScopeID(useScope)
      self.extendedScope = checker.bridgedScope(of: useScope)
    }

    /// Creates an instance for instantiating parameters used in `useScope` by a name referring to
    /// `r`, using `checker` to resolve extensions.
    init(_ r: DeclReference, in useScope: AnyScopeID, mutating checker: inout TypeChecker) {
      // References to constructors are always opened.
      if case .constructor(let d, _) = r {
        self.init(checker.program[checker.program[d].scope].scope, mutating: &checker)
        return
      }

      if let d = r.decl, checker.isRecursive(useOf: d, in: useScope) {
        self.init(checker.program[d].scope, mutating: &checker)
      } else {
        self.init(useScope, mutating: &checker)
      }
    }

  }

  /// Replaces the generic parameters in `subject` by fresh variables, forming instantiation
  /// constraints at `site`.
  mutating func open(type: AnyType, at site: SourceRange) -> InstantiatedType {
    // Since no generic parameter can be introduced at module scope, passing a module here will
    // force all parameters to be opened rather than skolemized.
    instantiate(type, in: AnyScopeID(ast.coreLibrary!), cause: .init(.structural, at: site))
  }

  /// Replaces the generic parameters in `candidate` by skolems or fresh variables depending on the
  /// relation between scope introducing them and `useScope`, assigning `cause` to instantiation
  /// constraints.
  mutating func instantiate(
    _ candidate: NameResolutionResult.Candidate, in useScope: AnyScopeID, cause: ConstraintOrigin
  ) -> NameResolutionResult.Candidate {
    let ctx = InstantiationContext(candidate.reference, in: useScope, mutating: &self)
    var substitutions: [GenericParameterDecl.ID: AnyType] = [:]

    let t = instantiate(candidate.type, in: ctx, cause: cause, updating: &substitutions)
    let r = candidate.reference.modifyingArguments(mutating: &substitutions) { (s, v) in
      let v = (v as? AnyType) ?? fatalError("not implemented")
      let x = instantiate(v, in: ctx, cause: cause, updating: &s)
      assert(x.constraints.isEmpty, "not implemented")
      return x.shape
    }

    assert(candidate.constraints.isEmpty, "not implemented")
    return .init(
      reference: r, type: t.shape, constraints: t.constraints, diagnostics: candidate.diagnostics)
  }

  /// Replaces the generic parameters in `subject` by skolems or fresh variables depending on
  /// whether their declaration is contained in `useScope`, assigning `cause` to instantiation
  /// constraints.
  mutating func instantiate(
    _ subject: AnyType, in useScope: AnyScopeID, cause: ConstraintOrigin
  ) -> InstantiatedType {
    let ctx = InstantiationContext(useScope, mutating: &self)
    var substitutions: [GenericParameterDecl.ID: AnyType] = [:]
    return instantiate(subject, in: ctx, cause: cause, updating: &substitutions)
  }

  /// Replaces the generic parameters in `subject` by skolems or fresh variables depending on
  /// whether their declaration is contained in `useScope` assigning `cause` to instantiation
  /// constraints and writing choices to `substitutions`.
  private func instantiate(
    _ subject: AnyType, in context: InstantiationContext, cause: ConstraintOrigin,
    updating substitutions: inout [GenericParameterDecl.ID: AnyType]
  ) -> InstantiatedType {
    func instantiate(type: AnyType) -> TypeTransformAction {
      switch type.base {
      case is AssociatedTypeType:
        fatalError("not implemented")

      case let p as GenericTypeParameterType:
        if let t = substitutions[p.decl] {
          return .stepOver(t)
        } else if shouldSkolemize(p, in: context) {
          return .stepOver(substitutions[p.decl].setIfNil(^SkolemType(quantifying: type)))
        } else {
          // TODO: Collect constraints
          return .stepOver(substitutions[p.decl].setIfNil(^TypeVariable()))
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

    let shape = subject.transform(instantiate(type:))
    return InstantiatedType(shape: shape, constraints: [])
  }

  /// Returns `true` iff `p` is declared in a scope containing `context`.
  private func shouldSkolemize(
    _ p: GenericTypeParameterType, in context: InstantiationContext
  ) -> Bool {
    let introductionScope = program[p.decl].scope

    if program.isContained(context.useScope, in: introductionScope) {
      return true
    } else if let s = context.extendedScope {
      return program.isContained(s, in: introductionScope)
    } else {
      return false
    }
  }

  /// Returns `true` iff a use of `d` in `useScope` is recursive.
  private func isRecursive(useOf d: AnyDeclID, in useScope: AnyScopeID) -> Bool {
    if let s = AnyScopeID(d) {
      return d.isCallable && program.isContained(useScope, in: s)
    } else {
      return false
    }
  }

  // MARK: AST Restructuring

  /// Returns a binary tree encoding the evaluation order of `e` or `nil` if `e` contains an
  /// undefined operator.
  mutating func fold(_ e: SequenceExpr.ID) -> FoldedSequenceExpr? {
    if let tree = foldedSequenceExprs[e] { return tree }

    let tree = fold(ast[e].tail[0...], into: .leaf(ast[e].head))
    foldedSequenceExprs[e] = tree
    return tree
  }

  /// Returns a copy of `initialResult` in which `tail` has been incorporated or `nil` if `tail`
  /// contains an undefined operator.
  private mutating func fold(
    _ tail: ArraySlice<SequenceExpr.TailElement>,
    into initialResult: FoldedSequenceExpr
  ) -> FoldedSequenceExpr? {
    var accumulator = initialResult

    for i in tail.indices {
      // Search for the operator declaration.
      let operatorStem = ast[tail[i].operator].name.value.stem
      let useScope = program[tail[i].operator].scope
      let candidates = lookup(operator: operatorStem, notation: .infix, exposedTo: useScope)

      switch candidates.count {
      case 0:
        report(.error(undefinedOperator: operatorStem, at: ast[tail[i].operator].site))
        return nil

      case 1:
        let precedence = ast[candidates[0]].precedenceGroup?.value
        accumulator.append(
          operator: .init(expr: tail[i].operator, precedence: precedence),
          right: tail[i].operand)

      default:
        // TODO: should probably emit a diagnostic. Operator declarations cannot be overloaded.
        fatalError("not implemented")
      }
    }

    return accumulator
  }

  // MARK: Utils

  /// Returns whether `d` is a nominal type declaration.
  mutating func isNominalTypeDecl(_ d: AnyDeclID) -> Bool {
    switch d.kind {
    case AssociatedTypeDecl.self, ProductTypeDecl.self, TypeAliasDecl.self:
      return true
    case GenericParameterDecl.self:
      return realize(genericParameterDecl: .init(d)!).base is MetatypeType
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
      let p = ProductTypeDecl.ID(program[d].scope)!
      return ast[p].members.reduce(into: ["self"]) { (l, m) in
        guard let b = BindingDecl.ID(m) else { return }
        l.append(
          contentsOf: ast.names(in: ast[b].pattern).map({ program[$0.pattern].decl.baseName }))
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
    switch n.kind {
    case InoutExpr.self:
      return visit(inoutExpr: .init(n)!, in: ast)
    case NameExpr.self:
      return visit(nameExpr: .init(n)!, in: ast)
    case ProductTypeDecl.self, TraitDecl.self, TypeAliasDecl.self:
      return false
    case ConformanceDecl.self, ExtensionDecl.self:
      return false
    default:
      return true
    }
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

extension DeclReference {

  /// Returns a copy of `self` in which the generic arguments of the referred declaration and that
  /// of its receiver (if any) have been mutated by applying `modify` to `m`.
  fileprivate func modifyingArguments<M>(
    mutating m: inout M, _ modify: (inout M, GenericArguments.Value) -> GenericArguments.Value
  ) -> Self {
    switch self {
    case .direct(let d, let a):
      return .direct(d, a.mapValues({ modify(&m, $0) }))
    case .member(let d, let a, let r):
      return .member(d, a.mapValues({ modify(&m, $0) }), r)
    case .constructor(let d, let a):
      return .constructor(d, a.mapValues({ modify(&m, $0) }))
    default:
      return self
    }
  }

}

extension DeclReference.Receiver {

  /// Returns a copy of `self` in which generic arguments (if any) have been mutated by applying
  /// `modify` to `m`.
  fileprivate func modifyingArguments<M>(
    mutating m: inout M, _ modify: (inout M, GenericArguments.Value) -> GenericArguments.Value
  ) -> Self {
    switch self {
    case .elided(let r):
      return .elided(r.modifyingArguments(mutating: &m, modify))
    default:
      return self
    }
  }

}
