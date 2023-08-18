import Core
import OrderedCollections
import Utils

/// The transformation from a `ScopedProgram` to a `TypedProgram`.
struct TypeChecker {

  /// The diagnostics of the type errors.
  private(set) var diagnostics = DiagnosticSet()

  /// An identifier for this instance, unique across concurrent type checking tasks.
  private let identifier: UInt8

  /// The identifier of the next fresh variable.
  private var nextFreshVariableIdentifier: UInt64 = 0

  /// The representation under construction.
  private var cache: Cache

  /// A closure that takes a node and its containing program, and returns `true` if a trace of type
  /// inference should be logged on the console for that node.
  private let shouldTraceInference: ((AnyNodeID, TypedProgram) -> Bool)?

  /// The local copy of the program being type checked.
  var program: TypedProgram {
    cache.local
  }

  /// Creates an instance as context for algorithms on `p`, whose property are already complete.
  init(asContextFor p: TypedProgram) {
    self.identifier = 0
    self.cache = Cache(local: p)
    self.shouldTraceInference = nil
  }

  /// Creates an instance for constructing `instanceUnderConstruction`.
  init(
    constructing instanceUnderConstruction: TypedProgram,
    tracingInferenceIf shouldTraceInference: ((AnyNodeID, TypedProgram) -> Bool)?
  ) {
    self.identifier = 0
    self.cache = Cache(local: instanceUnderConstruction)
    self.shouldTraceInference = shouldTraceInference
  }

  /// Creates an instance with given `identifier` for constructing `instanceUnderConstruction`
  /// collaboratively with other instances.
  ///
  /// - Requires: `identifier` is unique across collaborating instances.
  init(
    _ identifier: UInt8,
    collaborativelyConstructing instanceUnderConstruction: SharedMutable<TypedProgram>,
    tracingInferenceIf shouldTraceInference: ((AnyNodeID, TypedProgram) -> Bool)?
  ) {
    self.identifier = identifier
    self.nextFreshVariableIdentifier = UInt64(identifier) << 56
    self.cache = Cache(local: instanceUnderConstruction.wrapped, shared: instanceUnderConstruction)
    self.shouldTraceInference = shouldTraceInference
  }

  /// Reports the given diagnostic.
  private mutating func report(_ d: Diagnostic) {
    diagnostics.insert(d)
  }

  /// Reports the given diagnostics.
  private mutating func report<S: Sequence<Diagnostic>>(_ batch: S) {
    diagnostics.formUnion(batch)
  }

  // MARK: Type relations

  /// Returns the canonical form of `t` in `scopeOfUse`.
  mutating func canonical(_ t: AnyType, in scopeOfUse: AnyScopeID) -> AnyType {
    if t[.isCanonical] { return t }

    switch t.base {
    case let u as TypeAliasType:
      return canonical(u.resolved.value, in: scopeOfUse)
    case let u as BoundGenericType:
      return canonical(u, in: scopeOfUse)
    case let u as UnionType:
      return canonical(u, in: scopeOfUse)
    default:
      return t.transformParts(mutating: &self, { .stepOver($0.canonical($1, in: scopeOfUse)) })
    }
  }

  /// Returns the canonical form of `t` in `scopeOfUse`.
  private mutating func canonical(_ t: BoundGenericType, in scopeOfUse: AnyScopeID) -> AnyType {
    if t[.isCanonical] { return ^t }

    let b = canonical(t.base, in: scopeOfUse)
    let a = canonical(t.arguments, in: scopeOfUse)
    let s = specialize(b, for: a, in: scopeOfUse)
    return canonical(s, in: scopeOfUse)
  }

  /// Returns the canonical form of `t` in `scopeOfUse`.
  private mutating func canonical(_ t: UnionType, in scopeOfUse: AnyScopeID) -> AnyType {
    if t[.isCanonical] { return ^t }

    var elements = Set<AnyType>()
    for e in t.elements {
      elements.insert(canonical(e, in: scopeOfUse))
    }
    return elements.uniqueElement ?? ^UnionType(elements)
  }

  /// Returns `arguments` with all types replaced by their canonical form in `scopeOfUse`.
  mutating func canonical(
    _ arguments: GenericArguments, in scopeOfUse: AnyScopeID
  ) -> GenericArguments {
    arguments.mapValues { (v) in
      (v as? AnyType).map({ canonical($0, in: scopeOfUse) }) ?? v
    }
  }

  /// Returns `true` iff `t` and `u` are equivalent types in `scopeOfUse`.
  mutating func areEquivalent(_ t: AnyType, _ u: AnyType, in scopeOfUse: AnyScopeID) -> Bool {
    canonical(t, in: scopeOfUse) == canonical(u, in: scopeOfUse)
  }

  /// Returns the traits to which `t` is declared conforming in `scopeOfUse`.
  mutating func conformedTraits(of t: AnyType, in scopeOfUse: AnyScopeID) -> Set<TraitType> {
    switch t.base {
    case let u as BoundGenericType:
      return conformedTraits(of: u.base, in: scopeOfUse)
    case let u as GenericTypeParameterType:
      return conformedTraits(of: u, in: scopeOfUse)
    case let u as ProductType:
      return conformedTraits(of: u, in: scopeOfUse)
    case let u as TraitType:
      return conformedTraits(of: u, in: scopeOfUse)
    default:
      return conformedTraits(declaredInExtensionsOf: t, exposedTo: scopeOfUse)
    }
  }

  /// Returns the traits to which `t` is declared conforming in `scopeOfUse`.
  private mutating func conformedTraits(
    of t: GenericTypeParameterType, in scopeOfUse: AnyScopeID
  ) -> Set<TraitType> {
    // Generic parameters declared at trait scope conform to that trait.
    if let d = TraitDecl.ID(program[t.decl].scope) {
      return conformedTraits(of: TraitType(d, ast: program.ast), in: scopeOfUse)
    }

    // Conformances of other generic parameters are stored in generic environments.
    var result = Set<TraitType>()
    for s in program.scopes(from: scopeOfUse) where s.kind.value is GenericScope.Type {
      let e = environment(of: s)
      result.formUnion(e.conformedTraits(of: ^t))
    }

    result.formUnion(conformedTraits(declaredInExtensionsOf: ^t, exposedTo: scopeOfUse))
    return result
  }

  /// Returns the traits to which `t` is declared conforming in `scopeOfUse`.
  private mutating func conformedTraits(
    of t: ProductType, in scopeOfUse: AnyScopeID
  ) -> Set<TraitType> {
    var result = Set<TraitType>()
    for (_, u) in evalTraitComposition(program[t.decl].conformances) {
      result.formUnion(conformedTraits(of: u, in: scopeOfUse))
    }

    result.formUnion(conformedTraits(declaredInExtensionsOf: ^t, exposedTo: scopeOfUse))
    return result
  }

  /// Returns `concept` and the traits of which `concept` is a refinement in `scopeOfUse`.
  private mutating func conformedTraits(
    of concept: TraitType, in scopeOfUse: AnyScopeID
  ) -> Set<TraitType> {
    var result = Set([concept])

    for (n, t) in evalTraitComposition(program[concept.decl].refinements) {
      var work = Set([t])
      while let base = work.popFirst() {
        if base == concept {
          report(.error(circularRefinementAt: program[n].site))
        } else if result.insert(base).inserted {
          let new = evalTraitComposition(program[base.decl].refinements)
          work.formUnion(new.map(\.trait))
        }
      }
    }

    // Traits can't be refined in extensions; we're done.
    return result
  }

  /// Returns the traits to which `t` is declared conforming by conformance declarations exposed
  /// to `scopeOfUse`.
  private mutating func conformedTraits(
    declaredInExtensionsOf t: AnyType, exposedTo scopeOfUse: AnyScopeID
  ) -> Set<TraitType> {
    var result = Set<TraitType>()
    for e in extensions(of: t, exposedTo: scopeOfUse).filter(ConformanceDecl.self) {
      for (_, u) in evalTraitComposition(program[e].conformances) {
        result.formUnion(conformedTraits(of: u, in: scopeOfUse))
      }
    }
    return result
  }

  // MARK: Type transformations

  /// Returns `generic` with occurrences of parameters keying `specialization` replaced by their
  /// corresponding value, performing necessary name lookups from `scopeOfUse`.
  ///
  /// This method has no effect if `specialization` is empty.
  mutating func specialize(
    _ generic: AnyType, for specialization: GenericArguments, in scopeOfUse: AnyScopeID
  ) -> AnyType {
    return specialization.isEmpty ? generic : generic.transform(mutating: &self, transform)

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
      var candidates = me.lookup(me.program[t.decl].baseName, memberOf: d, exposedTo: scopeOfUse)

      // Ignore associated type declaration unless they define a default value that isn't
      // overridden in the conforming type.
      if let i = candidates.firstIndex(where: { $0.kind == AssociatedTypeDecl.self }) {
        if candidates.count > 1 {
          candidates.remove(at: i)
        } else if let a = me.program[AssociatedTypeDecl.ID(candidates[i])!].defaultValue {
          return me.evalTypeAnnotation(a)
        }
      }

      if let selected = candidates.uniqueElement {
        return MetatypeType(me.uncheckedType(of: selected))?.instance ?? .error
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
      if let v = specialization[t.decl] {
        return (v as? AnyType) ?? preconditionFailure("expected type")
      } else {
        return ^t
      }
    }

    /// If `t` is an unspecialized generic type, returns its specialization taking the arguments
    /// in `substitutions` corresponding to the parameters introduced by `d`. Otherwise, returns
    /// `t` unchanged.
    ///
    /// - Requires: `t` is not a trait.
    func transform<T: TypeProtocol, D: GenericScope>(
      mutating me: inout Self, _ t: T, declaredBy d: D.ID
    ) -> AnyType {
      assert(!(t is TraitType))
      let parameters = me.program[d].genericParameters
      if parameters.isEmpty {
        return ^t
      }

      var arguments: GenericArguments = [:]
      for p in parameters {
        arguments[p] = specialization[p] ?? ^me.freshVariable()
      }
      return ^BoundGenericType(t, arguments: arguments)
    }
  }

  /// Returns the type declared by `d` bound to open variables for each generic parameter
  /// introduced by `d`.
  ///
  /// - Requires: `d` is a a generic product type or type alias declaration.
  mutating func openForUnification(_ d: AnyDeclID) -> BoundGenericType {
    let parameters: [GenericParameterDecl.ID]
    if let t = ProductTypeDecl.ID(d) {
      parameters = program[t].genericClause!.value.parameters
    } else if let t = TypeAliasDecl.ID(d) {
      parameters = program.ast[t].genericClause!.value.parameters
    } else {
      preconditionFailure()
    }

    let b = MetatypeType(uncheckedType(of: d))!.instance
    let a = GenericArguments(
      uniqueKeysWithValues: parameters.map({ (key: $0, value: ^freshVariable()) }))
    return BoundGenericType(b, arguments: a)
  }

  // MARK: Type checking

  /// Applies all pending updates to the shared instance.
  mutating func synchronize() {
    cache.synchronize()
  }

  /// Type checks all declarations in `self.program`.
  mutating func checkAllDeclarations() {
    check(program.ast.modules)
  }

  /// Type checks the sources in `batch`.
  mutating func check<S: Sequence<TranslationUnit.ID>>(_ batch: S) {
    batch.forEach({ check($0) })
  }

  /// Type checks `u` and all declarations nested in `d`.
  mutating func check(_ u: TranslationUnit.ID) {
    // The core library is always implicitly imported.
    var imports = Set<ModuleDecl.ID>()
    if let m = program.ast.coreLibrary {
      imports.insert(m)
    }
    for d in program[u].decls.lazy.compactMap(ImportDecl.ID.init(_:)) {
      insertImport(d, from: u, in: &imports)
    }

    cache.write(imports, at: \.imports[u])
    check(program[u].decls)
  }

  /// Type checks the declarations in `batch`.
  private mutating func check<S: Sequence<T>, T: DeclID>(
    _ batch: S, ignoringSharedCache ignoreSharedCache: Bool = false
  ) {
    batch.forEach({ check($0, ignoringSharedCache: ignoreSharedCache) })
  }

  /// Type checks `d` and all declarations nested in `d`.
  ///
  /// Type checking is performed if and only if `cache[\.declType][d]` is `nil`. In this case, the
  /// type of `d` is computed with `uncheckedType(of: d)` and assigned to `cache[\.declType][d]`
  /// before type checking. Then, `_check(d)` is called with `cache.declsUnderChecking` containing
  /// `d`. After the call, `cache.declsUnderChecking` doesn't contain `d`. A diagnostic is reported
  /// if type checking failed.
  ///
  /// Type checking typically consists of visiting a declaration to generate proof obligations that
  /// are then discharged by solving type constrants and/or type checking additional declarations.
  ///
  /// - Parameters:
  ///   - d: The declaration to type check.
  ///
  /// - Requires: `!cache.declsUnderChecking.contains(d)`
  private mutating func check<T: DeclID>(
    _ d: T, ignoringSharedCache ignoreSharedCache: Bool = false
  ) {
    // Handle binding declarations as a special case.
    if let b = BindingDecl.ID(d) {
      checkedType(of: b, ignoringSharedCache: ignoreSharedCache)
      return
    }

    // Check if work has to be done.
    if cache.read(\.declType[d], ignoringSharedCache: ignoreSharedCache) != nil { return }

    // Prepare the declaration for type checking.
    let t = uncheckedType(of: d, ignoringSharedCache: ignoreSharedCache)
    if t.isError {
      cache.write(.error, at: \.declType[d], ignoringSharedCache: ignoreSharedCache)
      return
    }

    // Check for infinite recursion.
    if !cache.declsUnderChecking.insert(AnyDeclID(d)).inserted {
      fatalError("infinite recursion")
    }

    // Do the work.
    switch d.kind {
    case AssociatedTypeDecl.self:
      break
    case AssociatedValueDecl.self:
      break
    case ConformanceDecl.self:
      _check(ConformanceDecl.ID(d)!)
    case ExtensionDecl.self:
      _check(ExtensionDecl.ID(d)!)
    case FunctionDecl.self:
      _check(FunctionDecl.ID(d)!)
    case GenericParameterDecl.self:
      _check(GenericParameterDecl.ID(d)!)
    case ImportDecl.self:
      break
    case InitializerDecl.self:
      _check(InitializerDecl.ID(d)!)
    case MethodDecl.self:
      _check(MethodDecl.ID(d)!)
    case MethodImpl.self:
      _check(MethodImpl.ID(d)!)
    case ModuleDecl.self:
      _check(ModuleDecl.ID(d)!)
    case NamespaceDecl.self:
      _check(NamespaceDecl.ID(d)!)
    case OperatorDecl.self:
      _check(OperatorDecl.ID(d)!)
    case ParameterDecl.self:
      _check(ParameterDecl.ID(d)!)
    case ProductTypeDecl.self:
      _check(ProductTypeDecl.ID(d)!)
    case SubscriptDecl.self:
      _check(SubscriptDecl.ID(d)!)
    case SubscriptImpl.self:
      _check(SubscriptImpl.ID(d)!)
    case TraitDecl.self:
      _check(TraitDecl.ID(d)!)
    case TypeAliasDecl.self:
      _check(TypeAliasDecl.ID(d)!)
    default:
      unexpected(d, in: program.ast)
    }

    // Commit result to the cache.
    cache.declsUnderChecking.remove(AnyDeclID(d))
    cache.write(t, at: \.declType[d], ignoringSharedCache: ignoreSharedCache)
  }

  /// Type checks `d` and all declarations nested in `d`.
  private mutating func _check(_ d: ConformanceDecl.ID) {
    checkEnvironment(of: d)
    checkConformance(of: d, to: program[d].conformances)
    check(program[d].members)
  }

  /// Type checks `d` and all declarations nested in `d`.
  private mutating func _check(_ d: ExtensionDecl.ID) {
    checkEnvironment(of: d)
    check(program[d].members)
  }

  /// Type checks `d` and all declarations nested in `d`.
  ///
  /// - Requires: `d` is not the underlying declaration of a lambda.
  private mutating func _check(_ d: FunctionDecl.ID) {
    checkEnvironment(of: d)
    checkParameters(program[d].parameters, of: d)

    switch program[d].body {
    case .block(let b):
      check(b)

    case .expr(let b):
      let r = LambdaType(uncheckedType(of: d))!.output
      check(b, asBodyOfCallableProducing: r)

    case nil:
      // Only requirements and FFIs can be without a body.
      if !program.isRequirement(d) && !program[d].isForeignInterface {
        report(.error(declarationRequiresBodyAt: program[d].introducerSite))
      }
    }
  }

  /// Type checks `d` and all declarations nested in `d`.
  private mutating func _check(_ d: GenericParameterDecl.ID) {
    // TODO: Type check default values
  }

  /// Type checks `d` and all declarations nested in `d`.
  private mutating func _check(_ d: InitializerDecl.ID) {
    // Memberwize initializers trivially type check.
    if program[d].isMemberwise { return }

    // Note: receiver is type checked in `_uncheckedType(of:)`.
    checkEnvironment(of: d)
    checkParameters(program[d].parameters, of: d)

    if let b = program[d].body {
      check(b)
    } else if !program.isRequirement(d) {
      report(.error(declarationRequiresBodyAt: program[d].introducer.site))
    }
  }

  /// Type checks `d` and all declarations nested in `d`.
  private mutating func _check(_ d: MethodDecl.ID) {
    checkEnvironment(of: d)
    checkParameters(program[d].parameters, of: d)
    check(program[d].impls)
  }

  /// Type checks `d` and all declarations nested in `d`.
  private mutating func _check(_ d: MethodImpl.ID) {
    let t = MethodType(uncheckedType(of: MethodDecl.ID(program[d].scope)!))!
    cache.write(
      ^ParameterType(program[d].introducer.value, t.receiver),
      at: \.declType[program[d].receiver])

    switch program[d].body {
    case .block(let b):
      check(b)

    case .expr(let b):
      let r = LambdaType(uncheckedType(of: d))!.output
      check(b, asBodyOfCallableProducing: r)

    case nil:
      if !program.isRequirement(d) {
        report(.error(declarationRequiresBodyAt: program[d].introducer.site))
      }
    }
  }

  /// Type checks `d` and all declarations nested in `d`.
  private mutating func _check(_ d: ModuleDecl.ID) {
    program[d].sources.forEach({ check($0) })
  }

  /// Type checks `d` and all declarations nested in `d`.
  private mutating func _check(_ d: NamespaceDecl.ID) {
    check(program[d].members)
  }

  /// Type checks `d` and all declarations nested in `d`.
  private mutating func _check(_ d: OperatorDecl.ID) {
    let s = TranslationUnit.ID(program[d].scope)!
    for o in program[s].decls where o.kind == OperatorDecl.self {
      let other = OperatorDecl.ID(o)!
      if o != d,
        program[other].notation.value == program[d].notation.value,
        program[other].name.value == program[d].name.value
      {
        report(.error(duplicateOperatorNamed: program[d].name.value, at: program[d].site))
      }
    }
  }

  /// Type checks `d` and all declarations nested in `d`.
  private mutating func _check(_ d: ParameterDecl.ID) {
    if let e = program[d].defaultValue {
      guard let p = uncheckedType(of: d).errorFree else { return }
      _ = checkedType(of: e, asArgumentTo: ParameterType(p)!)
    }
  }

  /// Type checks `d` and all declarations nested in `d`.
  private mutating func _check(_ d: ProductTypeDecl.ID) {
    checkEnvironment(of: d)
    checkConformance(of: d, to: program[d].conformances)
    check(program[d].members)
  }

  /// Type checks `d` and all declarations nested in `d`.
  private mutating func _check(_ d: SubscriptDecl.ID) {
    checkEnvironment(of: d)
    checkParameters(program[d].parameters, of: d)
    check(program[d].impls)
  }

  /// Type checks `d` and all declarations nested in `d`.
  private mutating func _check(_ d: SubscriptImpl.ID) {
    if let r = program[d].receiver {
      let t = SubscriptType(uncheckedType(of: SubscriptDecl.ID(program[d].scope)!))!
      let u = RemoteType(t.captures[0].type)!
      cache.write(
        ^ParameterType(program[d].introducer.value, u.bareType),
        at: \.declType[r])
    }

    switch program[d].body {
    case .block(let b):
      check(b)

    case .expr(let b):
      let r = SubscriptImplType(uncheckedType(of: d))!.output
      check(b, asBodyOfCallableProducing: r)

    case nil:
      if !program.isRequirement(d) {
        report(.error(declarationRequiresBodyAt: program[d].introducer.site))
      }
    }
  }

  /// Type checks `d` and all declarations nested in `d`.
  private mutating func _check(_ d: TraitDecl.ID) {
    checkEnvironment(of: d)
    check(program[d].members)
    check(extensions(of: uncheckedType(of: d), exposedTo: program[d].scope))
  }

  private mutating func _check(_ d: TypeAliasDecl.ID) {
    checkEnvironment(of: d)
    let aliased = MetatypeType(uncheckedType(of: d))!.instance
    check(extensions(of: aliased, exposedTo: program[d].scope))
  }

  /// Type checks the parameters `ps` of `d`.
  ///
  /// - Requires: The parameters in `ps` have type annotations.
  private mutating func checkParameters<T: Decl & LexicalScope>(
    _ ps: [ParameterDecl.ID], of d: T.ID
  ) {
    var siblings = Set<String>()
    for p in ps {
      if siblings.insert(program[p].baseName).inserted {
        check(p)
      } else {
        report(.error(duplicateParameterNamed: program[p].baseName, at: program[p].site))
        cache.write(.error, at: \.declType[p])
      }
    }
  }

  /// Type checks `d` after its type has been inferred.
  ///
  /// This method is called from `discharge(obligations:relatedTo:)` on declarations whose types
  /// appear in the obligations to discharge.
  ///
  /// - Requires: `cache.uncheckedType[d]` has been computed.
  private mutating func checkPostInference<T: DeclID>(
    _ d: T, solution: Solution, ignoringSharedCache ignoreSharedCache: Bool
  ) {
    // Prepare the declaration for type checking.
    let t = reifyUncheckedType(of: d, withAssignmentsIn: solution)
    if t.isError {
      cache.write(.error, at: \.declType[d], ignoringSharedCache: ignoreSharedCache)
      return
    }

    switch d.kind {
    case FunctionDecl.self:
      checkPostInference(FunctionDecl.ID(d)!, solution: solution)
    case VarDecl.self:
      cache.write(t, at: \.declType[d], ignoringSharedCache: ignoreSharedCache)
    default:
      unexpected(d, in: program.ast)
    }
  }

  /// Type checks `d`, which declares a lambda, after its type has been inferred.
  private mutating func checkPostInference(_ d: FunctionDecl.ID, solution: Solution) {
    assert(program[d].isInExprContext, "expected lambda")
    for c in program[d].explicitCaptures {
      reifyUncheckedType(of: c, withAssignmentsIn: solution)
    }
    for p in program[d].parameters {
      reifyUncheckedType(of: p, withAssignmentsIn: solution)
    }
    check(d, ignoringSharedCache: true)
  }

  /// Substitutes open variables occurring in `cache.uncheckedType[d]` with their corresponding
  /// assignment in `solution`, returning `d`'s reified type.
  ///
  /// - Requires: `cache.uncheckedType[d]` has been computed.
  @discardableResult
  private mutating func reifyUncheckedType<T: DeclID>(
    of d: T, withAssignmentsIn solution: Solution
  ) -> AnyType {
    // Note: in theory, we should make sure this update is monotonic. In practice, such a test
    // would require prohibitively expensive structural comparisons.
    modify(&cache.uncheckedType[d]!) { (u) in
      let v = solution.typeAssumptions.reify(u.computed!)
      u = .computed(v)
      return v
    }
  }

  /// Type checks `e` as the body of a function returning or susbscript projecting `r`.
  private mutating func check(_ e: AnyExprID, asBodyOfCallableProducing r: AnyType) {
    var obligations = ProofObligations(scope: program[e].scope)

    let body = inferredType(of: e, withHint: r, updating: &obligations)

    // Inline functions may return `Never` regardless of their return type.
    let o = ConstraintOrigin(.return, at: program[e].site)
    let equalToNever = EqualityConstraint(body, .never, origin: o)

    if areEquivalent(r, .never, in: program[e].scope) {
      obligations.insert(equalToNever)
    } else {
      let c = DisjunctionConstraint(
        between: [
          .init(constraints: [SubtypingConstraint(body, r, origin: o)], penalties: 0),
          .init(constraints: [equalToNever], penalties: 1),
        ],
        origin: o)
      obligations.insert(c)
    }

    discharge(obligations, relatedTo: e)
  }

  /// Checks that the type of `e` is subtype of `supertype`.
  private mutating func check(_ e: AnyExprID, coercibleTo supertype: AnyType) {
    var obligations = ProofObligations(scope: program[e].scope)

    let t = inferredType(of: e, withHint: supertype, updating: &obligations)
    obligations.insert(
      SubtypingConstraint(t, supertype, origin: .init(.structural, at: program[e].site)))

    discharge(obligations, relatedTo: e)
  }

  /// Type checks `s`.
  private mutating func check<T: StmtID>(_ s: T) {
    switch s.kind {
    case AssignStmt.self:
      check(AssignStmt.ID(s)!)
    case BraceStmt.self:
      check(BraceStmt.ID(s)!)
    case ConditionalStmt.self:
      check(ConditionalStmt.ID(s)!)
    case ExprStmt.self:
      check(ExprStmt.ID(s)!)
    case DeclStmt.self:
      check(DeclStmt.ID(s)!)
    case DiscardStmt.self:
      check(DiscardStmt.ID(s)!)
    case DoWhileStmt.self:
      check(DoWhileStmt.ID(s)!)
    case ReturnStmt.self:
      check(ReturnStmt.ID(s)!)
    case WhileStmt.self:
      check(WhileStmt.ID(s)!)
    case YieldStmt.self:
      check(YieldStmt.ID(s)!)
    default:
      unexpected(s, in: program.ast)
    }
  }

  /// Type checks `s`.
  private mutating func check(_ s: BraceStmt.ID) {
    program[s].stmts.forEach({ check($0) })
  }

  /// Type checks `s`.
  private mutating func check(_ s: AssignStmt.ID) {
    var obligations = ProofObligations(scope: program[s].scope)
    let o = ConstraintOrigin(.initializationOrAssignment, at: program[s].site)

    // `lhs` must be `Movable`.
    let lhs = inferredType(of: program[s].left, updating: &obligations)
    obligations.insert(ConformanceConstraint(lhs, conformsTo: program.ast.movableTrait, origin: o))

    // `rhs` must be subtype of `lhs`.
    let rhs = inferredType(
      of: program[s].right, withHint: lhs, updating: &obligations)
    obligations.insert(SubtypingConstraint(rhs, lhs, origin: o))

    discharge(obligations, relatedTo: s)
  }

  /// Type checks `s`.
  private mutating func check(_ s: ConditionalStmt.ID) {
    check(program[s].condition)
    check(program[s].success)
    if let b = program[s].failure {
      check(b)
    }
  }

  /// Type checks `s`.
  private mutating func check(_ s: ExprStmt.ID) {
    guard let result = checkedType(of: program[s].expr).errorFree else { return }

    // Warn against unused result if the type of the expression is neither `Void` nor `Never`.
    let t = canonical(result, in: program[s].scope)
    if !t.isVoidOrNever {
      report(.warning(unusedResultOfType: result, at: program[s].expr.site))
    }
  }

  /// Type checks `s`.
  private mutating func check(_ s: DeclStmt.ID) {
    check(program[s].decl)
  }

  /// Type checks `s`.
  private mutating func check(_ s: DiscardStmt.ID) {
    var obligations = ProofObligations(scope: program[s].scope)
    let t = inferredType(of: program[s].expr, updating: &obligations)

    // Expression must be `Deinitializable`.
    obligations.insert(
      ConformanceConstraint(
        t, conformsTo: program.ast.deinitializableTrait,
        origin: .init(.discard, at: program[s].site)))

    discharge(obligations, relatedTo: s)
  }

  /// Type checks `s`.
  private mutating func check(_ s: DoWhileStmt.ID) {
    check(program[s].body)
    check(program[s].condition, coercibleTo: ^program.ast.coreType("Bool")!)
  }

  /// Type checks `s`.
  private mutating func check(_ s: ReturnStmt.ID) {
    let output = uncheckedOutputType(in: program[s].scope)!

    if let v = program[s].value {
      check(v, coercibleTo: output)
    } else if !areEquivalent(output, .void, in: program[s].scope) {
      report(.error(missingReturnValueAt: program[s].site))
    }
  }

  /// Type checks `s`.
  private mutating func check(_ s: WhileStmt.ID) {
    check(program[s].condition)
    check(program[s].body)
  }

  /// Type checks `s`.
  private mutating func check(_ s: YieldStmt.ID) {
    let output = uncheckedOutputType(in: program[s].scope)!
    check(program[s].value, coercibleTo: output)
  }

  /// Type checks `condition`.
  private mutating func check(_ condition: [ConditionItem]) {
    let bool = ^program.ast.coreType("Bool")!
    for item in condition {
      switch item {
      case .expr(let e):
        check(e, coercibleTo: bool)
      case .decl(let d):
        checkedType(of: d, usedAsCondition: true, ignoringSharedCache: true)
      }
    }
  }

  /// If `d` is a valid import in `u`, inserts the module referred by `d` in `imports`. Otherwise,
  /// a diagnostic is reported.
  private mutating func insertImport(
    _ d: ImportDecl.ID, from u: TranslationUnit.ID, in imports: inout Set<ModuleDecl.ID>
  ) {
    guard let m = ModuleType(uncheckedType(of: d)) else { return }

    if program.module(containing: u) != m.decl {
      imports.insert(m.decl)
    } else {
      report(.warning(needlessImport: d, in: program.ast))
    }
  }

  /// Builds and type checks the generic environment of `d`.
  private mutating func checkEnvironment<T: GenericDecl & LexicalScope>(of d: T.ID) {
    // TODO: Type check default values
    let e = environment(of: d)
    check(e.parameters)
  }

  /// Builds and type checks the generic environment of `d`.
  private mutating func checkEnvironment(of d: TraitDecl.ID) {
    // TODO: Type check default values
    let e = environment(of: d)
    check(e.parameters)
  }

  /// Builds and type checks the generic environment of `d`.
  private mutating func checkEnvironment<T: TypeExtendingDecl>(of d: T.ID) {
    // TODO: Type check default values
    let e = environment(of: d)
    check(e.parameters)
  }

  /// Type checks the conformances of the type declared by `d` to `concepts` and inserts valid ones
  /// in `self.conformances`, reporting diagnostics for each ill-typed conformance.
  private mutating func checkConformance<T: Decl & LexicalScope>(
    of d: T.ID, to concepts: [NameExpr.ID]
  ) {
    // Nothing to do if `concepts` is empty.
    if concepts.isEmpty { return }

    guard let r = resolveReceiverMetatype(in: AnyScopeID(d))!.instance.errorFree else { return }
    for (n, rhs) in evalTraitComposition(concepts) {
      for t in conformedTraits(of: rhs, in: program[d].scope) {
        _ = checkConformance(of: r, to: t, declaredBy: d, at: program[n].site)
      }
    }
  }

  /// Type checks the conformance of `model` to `concept`, which declared by `source` at `site`,
  /// returning it iff it is valid. Otherwise, returns `nil` and reports a diagnostic.
  private mutating func checkConformance<T: Decl>(
    of model: AnyType, to concept: TraitType,
    declaredBy source: T.ID, at site: SourceRange
  ) -> Conformance? {
    // Conformances at file scope are exposed in the whole module. Other conformances are exposed
    // in their containing scope.
    let scopeOfExposition = read(program[source].scope) { (s) in
      (s.kind == TranslationUnit.self) ? program[s].scope : s
    }

    // TODO: Verify requirement constraints
    // TODO: Use arguments to bound generic types as constraints

    var m = canonical(model, in: scopeOfExposition)
    let arguments: GenericArguments = [program[concept.decl].receiver: m]
    if let b = BoundGenericType(m) {
      m = b.base
    }

    if let s = cache.local.conformances[m, default: [:]][concept] {
      if let c = s.first(where: { $0.scope == scopeOfExposition }) { return c }
    }

    var implementations = Conformance.ImplementationMap()
    var conformanceDiagnostics = DiagnosticSet()

    for r in program.ast.requirements(of: concept.decl) {
      implementation(of: r)
    }

    if !conformanceDiagnostics.isEmpty {
      report(.error(model, doesNotConformTo: concept, at: site, because: conformanceDiagnostics))
      return nil
    }

    let c = Conformance(
      model: m, concept: concept,
      arguments: [:], conditions: [], scope: scopeOfExposition,
      implementations: implementations, isStructural: false, site: site)
    insertConformance(c)
    return c

    /// Returns the type of `d` viewed as a member of `model` through its conformance to `concept`
    /// in `scopeOfUse`, or `nil` no such type can be constructed.
    func type(ofMember m: AnyDeclID) -> AnyType {
      let t = uncheckedType(of: m)
      return specialize(t, for: arguments, in: scopeOfExposition)
    }

    /// Returns a concrete or syntheszed implementation of requirement `r` in `concept` for `model`
    /// exposed ot `scopeOfUse`, or `nil` if no such implementation exist.
    func implementation(of r: AnyDeclID) {
      // FIXME: remove me
      if r.kind == AssociatedTypeDecl.self { return }

      // Note: `t` is used for generating diagnostics, `u` is used for testing equivalences.
      let t = type(ofMember: r)
      let u = canonical(t, in: scopeOfExposition)

      let n = program.name(of: r)!
      if let d = concreteImplementation(of: r, typed: u, named: n) {
        implementations[r] = .concrete(d)
        return
      } else if let d = syntheticImplementation(of: r, typed: u, named: n) {
        implementations[r] = .synthetic(d)

        let m = program.module(containing: program[source].scope)
        var s = cache.local.synthesizedDecls[m] ?? []
        s.insert(d)
        cache.write(s, at: \.synthesizedDecls[m], ignoringSharedCache: true)
        return
      }

      conformanceDiagnostics.insert(
        .note(trait: concept, requires: r.kind, named: n, typed: t, at: site))
    }

    /// Returns a synthetic implementation of `requirement` in `concept`, which has type `t` and
    /// name `n` in `model`, or returns `nil` if no such implementation exist.
    func syntheticImplementation(
      of requirement: AnyDeclID, typed t: AnyType, named n: Name
    ) -> SynthesizedFunctionDecl? {
      if let k = program.ast.synthesizedKind(of: requirement, definedBy: concept) {
        // Note: compiler-known requirement is expected to be well-typed.
        let scopeOfDefinition = AnyScopeID(source) ?? program[source].scope
        return .init(k, typed: LambdaType(t)!, in: scopeOfDefinition)
      } else {
        return nil
      }
    }

    /// Returns a concrete implementation of `requirement` in `concept`, which has type `t` and
    /// name `n` in `model`, or returns `nil` if no such implementation exist.
    func concreteImplementation(
      of requirement: AnyDeclID, typed t: AnyType, named n: Name
    ) -> AnyDeclID? {
      switch requirement.kind {
      case AssociatedTypeDecl.self:
        fatalError("not implemented")

      case AssociatedValueDecl.self:
        fatalError("not implemented")

      case FunctionDecl.self:
        return implementation(
          of: requirement, typed: t, named: n,
          collectingCandidatesWith: collectFunction)

      case InitializerDecl.self:
        return implementation(
          of: requirement, typed: t, named: n, identifiedBy: InitializerDecl.ID.self,
          collectingCandidatesWith: appendIfDefinition)

      case MethodImpl.self:
        return implementation(
          of: requirement, typed: t, named: n,
          collectingCandidatesWith: collectFunction)

      case SubscriptImpl.self:
        fatalError("not implemented")

      default:
        unexpected(requirement, in: program.ast)
      }
    }

    /// Returns the implementation of `r` in `model`, with `r` a requirement of `concept` with type
    /// `t` and name `n`, or returns `nil` if no such implementation exist.
    func implementation<D: DeclID>(
      of requirement: AnyDeclID, typed t: AnyType, named n: Name,
      identifiedBy: D.Type = D.self,
      collectingCandidatesWith appendDefinitions: (D, AnyType, inout [AnyDeclID]) -> Void
    ) -> AnyDeclID? {
      guard !t[.hasError] else { return nil }

      let candidates = lookup(n.stem, memberOf: m, exposedTo: scopeOfExposition)
      let viable: [AnyDeclID] = candidates.reduce(into: []) { (s, c) in
        guard let d = D(c) else { return }
        appendDefinitions(d, t, &s)
      }

      return viable.uniqueElement
    }

    /// Appends to `s` the function definitions of `d` that have type `t`.
    func collectFunction(of d: AnyDeclID, matching t: AnyType, to s: inout [AnyDeclID]) {
      switch d.kind {
      case FunctionDecl.self:
        appendIfDefinition(FunctionDecl.ID(d)!, matching: t, in: &s)
      case MethodDecl.self:
        appendDefinitions(of: MethodDecl.ID(d)!, matching: t, in: &s)
      default:
        break
      }
    }

    /// Appends each variant of `c` to `candidates` that is has type `t`.
    func appendDefinitions(of d: MethodDecl.ID, matching t: AnyType, in s: inout [AnyDeclID]) {
      program[d].impls.forEach({ appendIfDefinition($0, matching: t, in: &s) })
    }

    /// Appends `d` to `s` iff it's a a definition with type `t`.
    func appendIfDefinition<D: Decl>(_ d: D.ID, matching t: AnyType, in s: inout [AnyDeclID]) {
      let u = type(ofMember: AnyDeclID(d))
      if program[d].isDefinition && areEquivalent(t, u, in: scopeOfExposition) {
        s.append(AnyDeclID(d))
      }
    }
  }

  /// Registers conformance `c` iff it hasn't been established.
  ///
  /// - Note: This method doesn't write to the shared cache.
  /// - Returns: `(true, c)` if no conformance describing how `c.model` conforms to `c.trait` in a
  ///   scope overlapping with `c.scope` was already registered. Otherwise, `(false, other)`, where
  ///   `other` is the existing conformance.
  @discardableResult
  private mutating func insertConformance(
    _ c: Conformance
  ) -> (inserted: Bool, conformanceAfterInsert: Conformance) {
    var traitToConformance = cache.local.conformances[c.model, default: [:]]
    let result = modify(&traitToConformance[c.concept, default: []]) { (s) in
      if let x = s.first(where: { program.areOverlapping($0.scope, c.scope) }) {
        return (inserted: false, conformanceAfterInsert: x)
      } else {
        let inserted = s.insert(c).inserted
        assert(inserted)
        return (inserted: true, conformanceAfterInsert: c)
      }
    }

    if result.inserted {
      cache.write(traitToConformance, at: \.conformances[c.model], ignoringSharedCache: true)
    }

    return result
  }

  /// Type checks `d` and all declarations nested in `d`, returning the type of `d`.
  ///
  /// - Requires: `!cache.declsUnderChecking.contains(d)`
  @discardableResult
  private mutating func checkedType(
    of d: BindingDecl.ID,
    usedAsCondition isConditional: Bool = false,
    ignoringSharedCache ignoreSharedCache: Bool = false
  ) -> AnyType {
    // Check if work has to be done.
    if let t = cache.read(\.declType[d], ignoringSharedCache: ignoreSharedCache) {
      return t
    }

    // Check for infinite recursion.
    if !cache.declsUnderChecking.insert(AnyDeclID(d)).inserted {
      fatalError("infinite recursion")
    }

    // The variable declarations nested in `d` are added to the set of declarations under checking
    // so that name resolution cannot bind a name to itself.
    var names = Set<AnyDeclID>()
    for (_, n) in program.ast.names(in: program[d].pattern) {
      names.insert(AnyDeclID(program[n].decl))
      cache.declsUnderChecking.insert(AnyDeclID(program[n].decl))
    }

    defer {
      cache.declsUnderChecking.remove(AnyDeclID(d))
      cache.declsUnderChecking.subtract(names)
      assert(names.allSatisfy({ cache.local.declType[$0] != nil }))
    }

    var obligations = ProofObligations(scope: program[d].scope)
    let t = inferredType(of: d, usedAsCondition: isConditional, updating: &obligations)
    let s = discharge(obligations, relatedTo: d)
    let u = s.typeAssumptions.reify(t)
    cache.write(s.isSound ? u : .error, at: \.declType[d], ignoringSharedCache: ignoreSharedCache)
    return u
  }

  /// Type checks `e` and returns its type, using `hint` as contextual type information.
  private mutating func checkedType(of e: AnyExprID, withHint hint: AnyType? = nil) -> AnyType {
    var obligations = ProofObligations(scope: program[e].scope)
    let t = inferredType(of: e, withHint: hint, updating: &obligations)
    let s = discharge(obligations, relatedTo: e)
    return s.typeAssumptions.reify(t)
  }

  /// Type checks `e` as an argument to `p` and returns its type.
  private mutating func checkedType(
    of e: AnyExprID, asArgumentTo p: ParameterType
  ) -> AnyType {
    var obligations = ProofObligations(scope: program[e].scope)

    let t = inferredType(of: e, withHint: p.bareType, updating: &obligations)
    obligations.insert(ParameterConstraint(t, ^p, origin: .init(.argument, at: program[e].site)))

    let s = discharge(obligations, relatedTo: e)
    return s.typeAssumptions.reify(t)
  }

  /// Returns the generic environment introduced by `s`.
  private mutating func environment(of s: AnyScopeID) -> GenericEnvironment {
    switch s.kind {
    case FunctionDecl.self:
      return environment(of: FunctionDecl.ID(s)!)
    case InitializerDecl.self:
      return environment(of: InitializerDecl.ID(s)!)
    case MethodDecl.self:
      return environment(of: MethodDecl.ID(s)!)
    case ProductTypeDecl.self:
      return environment(of: ProductTypeDecl.ID(s)!)
    case SubscriptDecl.self:
      return environment(of: SubscriptDecl.ID(s)!)
    case TypeAliasDecl.self:
      return environment(of: TypeAliasDecl.ID(s)!)
    case TraitDecl.self:
      return environment(of: TraitDecl.ID(s)!)
    default:
      unexpected(s, in: program.ast)
    }
  }

  /// Returns the generic environment introduced by `d`.
  private mutating func environment<T: GenericDecl & LexicalScope>(
    of d: T.ID
  ) -> GenericEnvironment {
    // Check if work has to be done.
    if let e = cache.read(\.environment[d]) { return e }

    // Nothing to do if the declaration has no generic clause.
    guard let clause = program[d].genericClause?.value else {
      return commit(GenericEnvironment(introducing: []))
    }

    var result = GenericEnvironment(introducing: clause.parameters)
    for p in clause.parameters {
      insertAnnotatedConstraints(on: p, in: &result)
    }
    for c in (clause.whereClause?.value.constraints ?? []) {
      insertConstraint(c, in: &result)
    }
    return commit(result)

    /// Commits `e` as the environment of `d` to the cache.
    func commit(_ e: GenericEnvironment) -> GenericEnvironment {
      cache.write(e, at: \.environment[d])
      return e
    }
  }

  /// Returns the generic environment introduced by `d`.
  private mutating func environment(of d: TraitDecl.ID) -> GenericEnvironment {
    // Check if work has to be done.
    if let e = cache.read(\.environment[d]) { return e }

    let receiver = program[d].receiver.id
    var result = GenericEnvironment(introducing: [receiver])

    for m in program[d].members {
      switch m.kind {
      case AssociatedTypeDecl.self:
        insertConstraints(of: AssociatedTypeDecl.ID(m)!, in: &result)
      case AssociatedValueDecl.self:
        insertConstraints(of: AssociatedValueDecl.ID(m)!, in: &result)
      default:
        continue
      }
    }

    // Synthesize `Self: T`.
    let s = GenericTypeParameterType(receiver, ast: program.ast)
    let t = TraitType(uncheckedType(of: d))!
    let c = GenericConstraint(
      .conformance(^s, conformedTraits(of: t, in: AnyScopeID(d))), at: program[d].identifier.site)
    result.insertConstraint(c)

    cache.write(result, at: \.environment[d])
    return result
  }

  /// Returns the generic environment introduced by `s`.
  private mutating func environment<T: TypeExtendingDecl>(of d: T.ID) -> GenericEnvironment {
    // Check if work has to be done.
    if let e = cache.read(\.environment[d]) { return e }

    var result = GenericEnvironment(introducing: [])
    for c in (program[d].whereClause?.value.constraints ?? []) {
      insertConstraint(c, in: &result)
    }

    cache.write(result, at: \.environment[d])
    return result
  }

  /// Insert's `d`'s constraints in `e`.
  private mutating func insertConstraints(
    of d: AssociatedTypeDecl.ID, in e: inout GenericEnvironment
  ) {
    insertAnnotatedConstraints(on: d, in: &e)
    for c in (program[d].whereClause?.value.constraints ?? []) {
      insertConstraint(c, in: &e)
    }
  }

  /// Insert's `d`'s constraints in `e`.
  private mutating func insertConstraints(
    of d: AssociatedValueDecl.ID, in e: inout GenericEnvironment
  ) {
    for c in (program[d].whereClause?.value.constraints ?? []) {
      insertConstraint(c, in: &e)
    }
  }

  /// Inserts the constraints declared as `p`'s annotations in `e`.
  private mutating func insertAnnotatedConstraints<T: ConstrainedGenericTypeDecl>(
    on p: T.ID, in e: inout GenericEnvironment
  ) {
    let t = uncheckedType(of: p)

    // TODO: Value constraints
    guard let lhs = MetatypeType(t)?.instance, lhs.base is GenericTypeParameterType
    else { return }

    // Synthesize sugared conformance constraint, if any.
    for (n, t) in evalTraitComposition(program[p].conformances) {
      let concepts = conformedTraits(of: t, in: program[p].scope)
      e.insertConstraint(.init(.conformance(lhs, concepts), at: program[n].site))
    }
  }

  /// Inserts `c` in `e`.
  private mutating func insertConstraint(
    _ c: SourceRepresentable<WhereClause.ConstraintExpr>, in e: inout GenericEnvironment
  ) {
    switch c.value {
    case .equality(let l, let r):
      guard
        let lhs = evalTypeAnnotation(l).errorFree,
        let rhs = evalTypeAnnotation(r).errorFree
      else { return }

      if lhs.isTypeParameter || rhs.isTypeParameter {
        e.insertConstraint(.init(.equality(lhs, rhs), at: c.site))
      } else {
        report(.error(invalidEqualityConstraintBetween: lhs, and: rhs, at: c.site))
      }

    case .conformance(let l, let r):
      guard let lhs = evalTypeAnnotation(l).errorFree else { return }
      guard lhs.isTypeParameter else {
        report(.error(invalidConformanceConstraintTo: lhs, at: c.site))
        return
      }

      var rhs: Set<TraitType> = []
      for (_, t) in evalTraitComposition(r) {
        rhs.formUnion(conformedTraits(of: t, in: program[l].scope))
      }
      e.insertConstraint(.init(.conformance(lhs, rhs), at: c.site))

    case .value(let p):
      // TODO: Symbolic execution
      return e.insertConstraint(.init(.predicate(p), at: c.site))
    }
  }

  /// Returns the type of `d`, computing it if necessary, without type checking `d`.
  ///
  /// This method returns the value for `d` in `cache[\.declType]` or `cache.uncheckedType` if
  /// it has been computed. Otherwise, it calls `_uncheckedType(d)` with `cache.uncheckedType[d]`
  /// assigned to `.inProgress`. After the call, `cache.uncheckedType[d] == .complete(t)`, where
  /// `t` is the return value. A diagnostic is reported if type the type of `d` contains an error.
  ///
  /// - Requires: `cache.uncheckedType[d] != .inProgress`
  private mutating func uncheckedType<T: DeclID>(
    of d: T, ignoringSharedCache ignoreSharedCache: Bool = false
  ) -> AnyType {
    // Check for infinite recursion.
    if let t = cache.uncheckedType[d] {
      guard let u = t.computed else { fatalError("infinite recursion") }
      return u
    } else {
      cache.uncheckedType[d] = .inProgress
    }

    // Check if work has to be done.
    if let t = cache.read(\.declType[d], ignoringSharedCache: ignoreSharedCache) {
      return commit(t)
    }

    // Do the work.
    switch d.kind {
    case AssociatedTypeDecl.self:
      return commit(_uncheckedType(of: AssociatedTypeDecl.ID(d)!))
    case AssociatedValueDecl.self:
      return commit(_uncheckedType(of: AssociatedValueDecl.ID(d)!))
    case BindingDecl.self:
      return commit(_uncheckedType(of: BindingDecl.ID(d)!))
    case ConformanceDecl.self:
      return commit(_uncheckedType(of: ConformanceDecl.ID(d)!))
    case ExtensionDecl.self:
      return commit(_uncheckedType(of: ExtensionDecl.ID(d)!))
    case FunctionDecl.self:
      return commit(_uncheckedType(of: FunctionDecl.ID(d)!))
    case GenericParameterDecl.self:
      return commit(_uncheckedType(of: GenericParameterDecl.ID(d)!))
    case ImportDecl.self:
      return commit(_uncheckedType(of: ImportDecl.ID(d)!))
    case InitializerDecl.self:
      return commit(_uncheckedType(of: InitializerDecl.ID(d)!))
    case MethodDecl.self:
      return commit(_uncheckedType(of: MethodDecl.ID(d)!))
    case MethodImpl.self:
      return commit(_uncheckedType(of: MethodImpl.ID(d)!))
    case ModuleDecl.self:
      return commit(_uncheckedType(of: ModuleDecl.ID(d)!))
    case NamespaceDecl.self:
      return commit(_uncheckedType(of: NamespaceDecl.ID(d)!))
    case OperatorDecl.self:
      return commit(_uncheckedType(of: OperatorDecl.ID(d)!))
    case ParameterDecl.self:
      return commit(_uncheckedType(of: ParameterDecl.ID(d)!))
    case ProductTypeDecl.self:
      return commit(_uncheckedType(of: ProductTypeDecl.ID(d)!))
    case SubscriptDecl.self:
      return commit(_uncheckedType(of: SubscriptDecl.ID(d)!))
    case SubscriptImpl.self:
      return commit(_uncheckedType(of: SubscriptImpl.ID(d)!))
    case TraitDecl.self:
      return commit(_uncheckedType(of: TraitDecl.ID(d)!))
    case TypeAliasDecl.self:
      return commit(_uncheckedType(of: TypeAliasDecl.ID(d)!))
    case VarDecl.self:
      return commit(_uncheckedType(of: VarDecl.ID(d)!))
    default:
      unexpected(d, in: program.ast)
    }

    /// Commits `t` as the unchecked type of `d` to the local cache.
    func commit(_ t: AnyType) -> AnyType {
      modify(&cache.uncheckedType[d]!) { (old) in
        if let u = old.computed { assert(u == t, "non-monotonic update") }
        old = .computed(t)
      }
      return t
    }
  }

  /// Computes and returns the type of `d`.
  private mutating func _uncheckedType(of d: AssociatedTypeDecl.ID) -> AnyType {
    // Parent scope must be a trait declaration.
    let traitDecl = TraitDecl.ID(program[d].scope)!
    let instance = AssociatedTypeType(
      NodeID(d)!,
      domain: ^GenericTypeParameterType(selfParameterOf: traitDecl, in: program.ast),
      ast: program.ast)
    return ^MetatypeType(of: instance)
  }

  /// Computes and returns the type of `d`.
  private mutating func _uncheckedType(of d: AssociatedValueDecl.ID) -> AnyType {
    // Parent scope must be a trait declaration.
    let traitDecl = TraitDecl.ID(program[d].scope)!
    let instance = AssociatedValueType(
      NodeID(d)!,
      domain: ^GenericTypeParameterType(selfParameterOf: traitDecl, in: program.ast),
      ast: program.ast)
    return ^MetatypeType(of: instance)
  }

  /// Computes and returns the type of `d`.
  private mutating func _uncheckedType(of d: BindingDecl.ID) -> AnyType {
    checkedType(of: d)
  }

  /// Computes and returns the type of `d`.
  private mutating func _uncheckedType<T: TypeExtendingDecl>(of d: T.ID) -> AnyType {
    let (i, cs) = eval(existentialBound: program[d].subject)
    assert(cs.isEmpty, "not implemented")

    switch i.base {
    case is BuiltinType, is RemoteType:
      report(.error(cannotExtend: i, at: program[d].subject.site))
    default:
      return i
    }

    return .error
  }

  /// Computes and returns the type of `d`.
  private mutating func _uncheckedType(of d: FunctionDecl.ID) -> AnyType {
    let inputs = uncheckedInputTypes(of: program[d].parameters, declaredBy: d)
    let output = evalReturnTypeAnnotation(of: d)

    if program.isNonStaticMember(d) {
      let k = program[d].receiverEffect?.value ?? .let
      let r = resolveReceiverMetatype(in: program[d].scope)!.instance
      cache.write(^ParameterType(k, r), at: \.declType[program[d].receiver!])

      let e = TupleType([.init(label: "self", type: k == .sink ? r : ^RemoteType(k, r))])
      return ^LambdaType(receiverEffect: k, environment: ^e, inputs: inputs, output: output)
    }

    assert(program[d].receiver == nil)
    let captures = uncheckedCaptureTypes(of: d)
    let e = TupleType(captures.explict + captures.implicit)
    return ^LambdaType(environment: ^e, inputs: inputs, output: output)
  }

  /// Computes and returns the type of `d`.
  private mutating func _uncheckedType(of d: GenericParameterDecl.ID) -> AnyType {
    let bounds = program[d].conformances.map({ evalTypeAnnotation($0) })

    // The declaration introduces a value if it's first annotation isn't a trait. Otherwise, it
    // introduces a type.
    if let first = bounds.first, !(first.base is TraitType) {
      if bounds.count > 1 {
        let s = program[program[d].conformances[1]].site
        report(.error(tooManyAnnotationsOnGenericValueParametersAt: s))
      }
      return first
    } else {
      return ^MetatypeType(of: GenericTypeParameterType(d, ast: program.ast))
    }
  }

  /// Computes and returns the type of `d`.
  private mutating func _uncheckedType(of d: ImportDecl.ID) -> AnyType {
    guard let m = program.ast.module(named: program[d].baseName) else {
      report(.error(noSuchModule: program[d].baseName, at: program[d].identifier.site))
      return .error
    }
    return ^ModuleType(m, ast: program.ast)
  }

  /// Computes and returns the type of `d`.
  private mutating func _uncheckedType(of d: InitializerDecl.ID) -> AnyType {
    if program[d].isMemberwise {
      let productTypeDecl = ProductTypeDecl.ID(program[d].scope)!
      let t = memberwiseInitializer(of: productTypeDecl)
      cache.write(t.inputs[0].type, at: \.declType[program[d].receiver])
      return ^t
    }

    let r = resolveReceiverMetatype(in: program[d].scope)!.instance
    let t = ^ParameterType(.set, r)
    cache.write(t, at: \.declType[program[d].receiver])

    let i = CallableTypeParameter(label: "self", type: t)
    let inputs = uncheckedInputTypes(of: program[d].parameters, declaredBy: d)
    return ^LambdaType(environment: .void, inputs: [i] + inputs, output: .void)
  }

  /// Computes and returns the type of `d`.
  private mutating func _uncheckedType(of d: MethodDecl.ID) -> AnyType {
    let inputs = uncheckedInputTypes(of: program[d].parameters, declaredBy: d)
    let output = evalReturnTypeAnnotation(of: d)

    let r = resolveReceiverMetatype(in: program[d].scope)!.instance
    return ^MethodType(
      capabilities: .init(program[d].impls.map({ program[$0].introducer.value })),
      receiver: r,
      inputs: inputs,
      output: output)
  }

  /// Computes and returns the type of `d`.
  private mutating func _uncheckedType(of d: MethodImpl.ID) -> AnyType {
    guard let bundle = MethodType(uncheckedType(of: AnyDeclID(program[d].scope)!)) else {
      return .error
    }

    let k = program[d].introducer.value
    assert(bundle.capabilities.contains(k))

    let r = bundle.receiver
    cache.write(^ParameterType(k, r), at: \.declType[program[d].receiver])

    let e: AnyType
    let o: AnyType
    switch k {
    case .let:
      e = ^TupleType([.init(label: "self", type: ^RemoteType(k, r))])
      o = bundle.output

    case .sink:
      e = ^TupleType([.init(label: "self", type: r)])
      o = bundle.output

    case .set, .inout:
      guard let t = mutatingVariantOutput(of: bundle, in: program[d].scope) else {
        diagnostics.insert(.error(mutatingBundleMustReturnTupleAt: program[d].introducer.site))
        return .error
      }
      e = ^TupleType([.init(label: "self", type: ^RemoteType(k, r))])
      o = t

    case .yielded:
      unreachable()
    }

    return ^LambdaType(receiverEffect: k, environment: e, inputs: bundle.inputs, output: o)
  }

  /// Computes and returns the type of `d`.
  private mutating func _uncheckedType(of d: ModuleDecl.ID) -> AnyType {
    ^ModuleType(d, ast: program.ast)
  }

  /// Computes and returns the type of `d`.
  private mutating func _uncheckedType(of d: OperatorDecl.ID) -> AnyType {
    .void
  }

  /// Computes and returns the type of `d`.
  private mutating func _uncheckedType(of d: NamespaceDecl.ID) -> AnyType {
    ^NamespaceType(d, ast: program.ast)
  }

  /// Computes and returns the type of `d`.
  ///
  /// - Requires: `d` has a type annotation.
  private mutating func _uncheckedType(of d: ParameterDecl.ID) -> AnyType {
    let a = program[d].annotation ?? preconditionFailure("missing type annotation")
    let t = evalTypeAnnotation(a)

    // The annotation may not omit generic arguments.
    if t[.hasVariable] {
      report(.error(notEnoughContextToInferArgumentsAt: program[a].site))
      return t.replacingVariablesWithErrors
    }

    return t
  }

  /// Computes and returns the type of `d`.
  private mutating func _uncheckedType(of d: ProductTypeDecl.ID) -> AnyType {
    ^MetatypeType(of: ProductType(d, ast: program.ast))
  }

  /// Computes and returns the type of `d`.
  private mutating func _uncheckedType(of d: SubscriptDecl.ID) -> AnyType {
    let inputs = uncheckedInputTypes(of: program[d].parameters, declaredBy: d)
    let output = evalTypeAnnotation(program[d].output)

    let e: TupleType
    if program.isNonStaticMember(d) {
      let r = resolveReceiverMetatype(in: program[d].scope)!.instance
      e = TupleType([.init(label: "self", type: ^RemoteType(.yielded, r))])
    } else {
      let captures = uncheckedCaptureTypes(of: d)
      e = TupleType(captures.explict + captures.implicit)
    }

    return ^SubscriptType(
      isProperty: program[d].isProperty,
      capabilities: .init(program[d].impls.map({ program[$0].introducer.value })),
      environment: ^e,
      inputs: inputs,
      output: output)
  }

  /// Computes and returns the type of `d`.
  private mutating func _uncheckedType(of d: SubscriptImpl.ID) -> AnyType {
    guard let bundle = SubscriptType(uncheckedType(of: AnyDeclID(program[d].scope)!)) else {
      return .error
    }

    let k = program[d].introducer.value
    assert(bundle.capabilities.contains(k))

    let t = bundle.transformParts { (t) in
      switch t.base {
      case let u as ParameterType where u.access == .yielded:
        return .stepInto(^ParameterType(k, u.bareType))
      case let u as RemoteType where u.access == .yielded:
        return .stepInto(^RemoteType(k, u.bareType))
      default:
        return .stepInto(t)
      }
    }

    if let s = program[d].receiver {
      let r = RemoteType(t.captures[0].type)!
      cache.write(^ParameterType(r), at: \.declType[s])
    }

    return ^SubscriptImplType(
      isProperty: t.isProperty,
      receiverEffect: k,
      environment: t.environment,
      inputs: t.inputs,
      output: t.output)
  }

  /// Computes and returns the type of `d`.
  private mutating func _uncheckedType(of d: TraitDecl.ID) -> AnyType {
    ^TraitType(d, ast: program.ast)
  }

  /// Computes and returns the type of `d`.
  private mutating func _uncheckedType(of d: TypeAliasDecl.ID) -> AnyType {
    let t = evalTypeAnnotation(program[d].aliasedType)
    let a = TypeAliasType(aliasing: t, declaredBy: d, in: program.ast)
    return ^MetatypeType(of: a)
  }

  /// Computes and returns the type of `d`.
  private mutating func _uncheckedType(of d: VarDecl.ID) -> AnyType {
    check(program[d].binding, ignoringSharedCache: true)
    return cache.local.declType[d]!
  }

  /// Computes and returns the types of `d`'s captures.
  ///
  /// - Requires: `d` is not a member declaration.
  private mutating func uncheckedCaptureTypes<T: CapturingDecl>(
    of d: T.ID
  ) -> (explict: [TupleType.Element], implicit: [TupleType.Element]) {
    let e = explicitCaptures(program[d].explicitCaptures, of: d)
    let i = implicitCaptures(of: d, ignoring: Set(e.map(\.label!)))
    return (e, i)
  }

  /// Computes and returns the type of `d`.
  ///
  /// - Requires: `d` has a type annotation.
  private mutating func uncheckedInputType(of d: ParameterDecl.ID) -> CallableTypeParameter {
    .init(
      label: program[d].label?.value,
      type: uncheckedType(of: d, ignoringSharedCache: true),
      hasDefault: program[d].defaultValue != nil)
  }

  /// Computes and returns the types of the inputs `ps` of `d`.
  ///
  /// - Requires: The parameters in `ps` have type annotations.
  private mutating func uncheckedInputTypes<T: Decl & LexicalScope>(
    of ps: [ParameterDecl.ID], declaredBy d: T.ID
  ) -> [CallableTypeParameter] {
    var result: [CallableTypeParameter] = []
    for p in ps {
      precondition(program[p].annotation != nil)
      let i = CallableTypeParameter(
        label: program[p].label?.value,
        type: uncheckedType(of: p, ignoringSharedCache: true),
        hasDefault: program[p].defaultValue != nil)
      result.append(i)
    }
    return result
  }

  /// Computes and returns the types of the inputs of `e`'s underlying declaration, using `hint`
  /// to guess the passing conventions of unnaotated parameters.
  ///
  /// `hint` is used as contextual information to refine guesses iff it is a lambda type with the
  /// same number of parameters as `e`. Parameter annotations take precedence in case of conflict.
  ///
  /// After the call, `cache.uncheckedType[p]` may be assigned to a type variable if `p` has no
  /// type annotation. Type checking is expected to reify such variables once the type of the
  /// expression in which `e` occurs has been checked.
  private mutating func uncheckedInputTypes(
    of e: LambdaExpr.ID, withHint hint: LambdaType?
  ) -> [CallableTypeParameter] {
    let d = program[e].decl.id
    let ps = program[d].parameters

    // Fast path: all parameters have an annotation.
    if ps.allSatisfy({ program[$0].annotation != nil }) {
      return uncheckedInputTypes(of: ps, declaredBy: d)
    }

    // Slow path: infer elided conventions from the uses in the lambda's body.
    let inputHints = hint.flatMap({ $0.inputs.count == ps.count ? $0.inputs : nil })
    let inferredConventions = uncheckedPassingConventions(of: e, withHint: inputHints)
    var result: [CallableTypeParameter] = []

    for (i, p) in ps.enumerated() {
      let t: AnyType
      if program[p].annotation != nil {
        t = uncheckedType(of: p, ignoringSharedCache: true)
      } else if let u = cache.uncheckedType[p]?.computed {
        t = u
      } else {
        t = inputHints?[i].type ?? ^ParameterType(inferredConventions[i], ^freshVariable())
        cache.uncheckedType[p] = .computed(t)
      }
      result.append(.init(label: program[p].label?.value, type: t))
    }
    return result
  }

  /// Returns the passing conventions of `e`'s parameters inferred from uses in `e`'s body.
  ///
  /// Parameters used immutably is inferred to be passed `let` unless corresponding hints specify
  /// a different convention. Parameters used mutably are inferred to be passed `inout` unless
  /// corresponding hints specify a convention other than `let`.
  ///
  /// - Requires: `hint` is `nil` or contains has as many elements as `e`'s parameters.
  private mutating func uncheckedPassingConventions(
    of e: LambdaExpr.ID, withHint hint: [CallableTypeParameter]?
  ) -> [AccessEffect] {
    let ps = program[e].decl.parameters
    var result: [AccessEffect]
    if let h = hint {
      result = h.map({ ParameterType($0.type)?.access ?? .let })
    } else {
      result = .init(repeating: .let, count: ps.count)
    }

    // Look at uses to update conventions where we could only guess `let` from the context.
    for (n, m) in program.ast.uses(in: AnyDeclID(program[e].decl)) {
      let candidates = lookup(unqualified: program[n].name.value.stem, in: program[n].scope)
      guard
        let pick = candidates.unique(ParameterDecl.self),
        let i = ps.firstIndex(of: pick),
        result[i] == .let
      else { continue }
      result[i] = m
    }

    return result
  }

  /// Computes and returns the type of values expected to be returned or projected in `scopeOfUse`,
  /// or returns `nil` if `scopeOfUse` is not nested in the declaration of a callable entity.
  private mutating func uncheckedOutputType(in scopeOfUse: AnyScopeID) -> AnyType? {
    switch scopeOfUse.kind {
    case ModuleDecl.self:
      return nil
    case FunctionDecl.self:
      return uncheckedOutputType(of: FunctionDecl.ID(scopeOfUse)!)
    case InitializerDecl.self:
      return .void
    case MethodImpl.self:
      return uncheckedOutputType(of: MethodImpl.ID(scopeOfUse)!)
    case SubscriptImpl.self:
      return uncheckedOutputType(of: SubscriptImpl.ID(scopeOfUse)!)
    default:
      return uncheckedOutputType(in: program[scopeOfUse].scope)
    }
  }

  /// Computes and returns the type of values returned by `d`.
  private mutating func uncheckedOutputType(of d: FunctionDecl.ID) -> AnyType {
    LambdaType(uncheckedType(of: d))?.output ?? .error
  }

  /// Computes and returns the type of values returned by `d`.
  private mutating func uncheckedOutputType(of d: MethodImpl.ID) -> AnyType {
    LambdaType(uncheckedType(of: d))?.output ?? .error
  }

  /// Computes and returns the type of values projected by `d`.
  private mutating func uncheckedOutputType(of d: SubscriptImpl.ID) -> AnyType {
    SubscriptImplType(uncheckedType(of: d))?.output ?? .error
  }

  /// Returns the types of the explicit captures `cs` of `d`.
  private mutating func explicitCaptures<T: Decl & LexicalScope>(
    _ cs: [BindingDecl.ID], of d: T.ID
  ) -> [TupleType.Element] {
    var result: [TupleType.Element] = []
    var sibligns = Set<String>()

    for c in cs {
      let t = uncheckedType(of: c, ignoringSharedCache: true)
      for (_, n) in program.ast.names(in: program[c].pattern) {
        if !sibligns.insert(program[n].decl.baseName).inserted {
          report(.error(duplicateCaptureNamed: program[n].decl.baseName, at: program[n].site))
          continue
        }

        let label = program[n].decl.identifier.value
        switch program[c].pattern.introducer.value {
        case .let:
          result.append(.init(label: label, type: ^RemoteType(.let, t)))
        case .inout:
          result.append(.init(label: label, type: ^RemoteType(.inout, t)))
        case .sinklet, .var:
          result.append(.init(label: label, type: ^t))
        }
      }
    }

    return result
  }

  /// Returns the implicit captures found in the body of `d`.
  private mutating func implicitCaptures<T: Decl & LexicalScope>(
    of d: T.ID, ignoring explictCaptures: Set<String>
  ) -> [TupleType.Element] {
    // Only local declarations have captures.
    if !program.isLocal(d) {
      cache.write([], at: \.implicitCaptures[d])
      return []
    }

    var captureToStemAndEffect: [AnyDeclID: (stem: String, effect: AccessEffect)] = [:]
    for (name, mutability) in program.ast.uses(in: AnyDeclID(d)) {
      guard
        let (stem, pick) = resolveImplicitCapture(name, occuringIn: d),
        !explictCaptures.contains(stem)
      else { continue }

      modify(&captureToStemAndEffect[pick, default: (stem, .let)]) { (x) in
        x.effect = max(x.effect, mutability)
      }
    }

    var captures: Set<ImplicitCapture> = []
    var types: [TupleType.Element] = []
    for (d, x) in captureToStemAndEffect {
      let t = RemoteType(x.effect, uncheckedType(of: d))
      captures.insert(ImplicitCapture(name: .init(stem: x.stem), type: t, decl: d))
      types.append(.init(label: x.stem, type: ^t))
    }

    cache.write(captures, at: \.implicitCaptures[d])
    return types
  }

  /// Returns type of `d`'s memberwise initializer.
  private mutating func memberwiseInitializer(of d: ProductTypeDecl.ID) -> LambdaType {
    let r = resolveReceiverMetatype(in: d)!.instance
    var inputs = [CallableTypeParameter(label: "self", type: ^ParameterType(.set, r))]

    for m in program[d].members.filter(BindingDecl.self) {
      check(m)
      for (_, n) in program.ast.names(in: program[m].pattern) {
        let d = program[n].decl.id
        let t = ParameterType(.sink, cache[\.declType[d]]!)
        inputs.append(.init(label: program[d].baseName, type: ^t))
      }
    }

    return LambdaType(environment: .void, inputs: inputs, output: .void)
  }

  // MARK: Evaluation

  /// Evaluates and returns the value of `e`.
  ///
  /// Use this method to evaluate compile-time expressions in value contexts, such as arguments
  /// to generic parameters.
  private mutating func eval(_ e: AnyExprID) -> any CompileTimeValue {
    let t = checkedType(of: e)
    return SymbolicValue(staticType: t)
  }

  /// Evaluates and returns the value of `e`, which is a type annotation.
  private mutating func evalTypeAnnotation(_ e: AnyExprID) -> AnyType {
    switch e.kind {
    case ConformanceLensTypeExpr.self:
      return evalTypeAnnotation(ConformanceLensTypeExpr.ID(e)!)
    case ExistentialTypeExpr.self:
      return evalTypeAnnotation(ExistentialTypeExpr.ID(e)!)
    case LambdaTypeExpr.self:
      return evalTypeAnnotation(LambdaTypeExpr.ID(e)!)
    case NameExpr.self:
      return evalTypeAnnotation(NameExpr.ID(e)!)
    case ParameterTypeExpr.self:
      return evalTypeAnnotation(ParameterTypeExpr.ID(e)!)
    case RemoteExpr.self:
      return evalTypeAnnotation(RemoteExpr.ID(e)!)
    case TupleTypeExpr.self:
      return evalTypeAnnotation(TupleTypeExpr.ID(e)!)
    case WildcardExpr.self:
      return evalTypeAnnotation(WildcardExpr.ID(e)!)
    default:
      break
    }

    // Attempt to evaluate `e` as a metatype.
    let v = eval(e)
    if let t = MetatypeType(v.staticType) {
      return t.instance
    } else {
      report(.error(typeExprDenotesValue: e, in: program.ast))
      return .error
    }
  }

  /// Evaluates and returns the value of `e`, which is a type annotation.
  private mutating func evalTypeAnnotation(_ e: ConformanceLensTypeExpr.ID) -> AnyType {
    guard
      let t = evalTypeAnnotation(program[e].lens).errorFree,
      let s = evalTypeAnnotation(program[e].subject).errorFree
    else { return .error }

    guard let lens = TraitType(t) else {
      report(.error(notATrait: t, at: program[e].lens.site))
      return .error
    }

    guard conformedTraits(of: s, in: program[e].scope).contains(lens) else {
      report(.error(s, doesNotConformTo: lens, at: program[e].lens.site))
      return .error
    }

    return ^ConformanceLensType(viewing: s, through: lens)
  }

  /// Evaluates and returns the value of `e`, which is a type annotation.
  private mutating func evalTypeAnnotation(_ e: ExistentialTypeExpr.ID) -> AnyType {
    let (i, cs) = eval(existentialInterface: program[e].traits)

    assert(cs.isEmpty, "not implemented")
    assert(program[e].whereClause == nil, "not implemented")

    return ^ExistentialType(i, constraints: cs)
  }

  /// Evaluates and returns the value of `e`, which is a type annotation.
  private mutating func evalTypeAnnotation(_ e: LambdaTypeExpr.ID) -> AnyType {
    let environment: AnyType
    if let v = program[e].environment {
      environment = evalTypeAnnotation(v)
    } else {
      environment = .any
    }

    let inputs = evalParameterAnnotations(of: e)
    let output = evalTypeAnnotation(program[e].output)

    return ^LambdaType(
      receiverEffect: program[e].receiverEffect?.value ?? .let,
      environment: environment,
      inputs: inputs,
      output: output)
  }

  /// Evaluates and returns the value of `e`, which is a type annotation.
  private mutating func evalTypeAnnotation(_ e: NameExpr.ID) -> AnyType {
    let resolution = resolve(e, withNonNominalPrefix: { (me, p) in me.evalQualification(of: p) })
    switch resolution {
    case .done(let prefix, let suffix) where suffix.isEmpty:
      // Nominal type expressions shall not be overloaded.
      guard let candidate = prefix.last!.candidates.uniqueElement else {
        report(.error(ambiguousUse: prefix.last!.component, in: program.ast))
        return .error
      }

      // Last component must resolve to a type or trait.
      switch candidate.type.base {
      case is MetatypeType, is TraitType:
        break
      case is ErrorType:
        return .error
      default:
        report(.error(typeExprDenotesValue: prefix.last!.component, in: program.ast))
        return .error
      }

      // FIXME: Should we update `referredDecl`?
      let c = prefix.last!.candidates[0]
      if isBoundToNominalTypeDecl(c.reference) {
        return MetatypeType(c.type)!.instance
      } else {
        return c.type
      }

    case .failed:
      return .error

    case .canceled:
      // Non-nominal prefixes are handled by the closure passed to `resolveNominalPrefix`.
      unreachable()

    default:
      fatalError("not implemented")
    }
  }

  /// Evaluates and returns the value of `e`, which is a type annotation.
  private mutating func evalTypeAnnotation(_ e: ParameterTypeExpr.ID) -> AnyType {
    let t = evalTypeAnnotation(program[e].bareType)
    return ^ParameterType(program[e].convention.value, t)
  }

  /// Evaluates and returns the value of `e`, which is a type annotation.
  private mutating func evalTypeAnnotation(_ e: RemoteExpr.ID) -> AnyType {
    let t = evalTypeAnnotation(program[e].operand)
    return ^RemoteType(program[e].convention.value, t)
  }

  /// Evaluates and returns the value of `e`, which is a type annotation.
  private mutating func evalTypeAnnotation(_ e: TupleTypeExpr.ID) -> AnyType {
    var elements: [TupleType.Element] = []
    for m in program[e].elements {
      let t = evalTypeAnnotation(m.type)
      elements.append(.init(label: m.label?.value, type: t))
    }
    return ^TupleType(elements)
  }

  /// Evaluates and returns the value of `e`, which is a type annotation.
  private mutating func evalTypeAnnotation(_ e: WildcardExpr.ID) -> AnyType {
    ^freshVariable()
  }

  /// Evaluates and returns the qualification of `e`, which is a type annotation.
  private mutating func evalQualification(of e: NameExpr.ID) -> AnyType? {
    switch program[e].domain {
    case .explicit(let q):
      return evalTypeAnnotation(q)
    case .implicit:
      report(.error(notEnoughContextToResolveMember: program[e].name))
      return .error
    case .none, .operand:
      unreachable()
    }
  }

  /// Evalutes and returns the parameter annotations of `e`.ns.
  private mutating func evalParameterAnnotations(
    of e: LambdaTypeExpr.ID
  ) -> [CallableTypeParameter] {
    var result: [CallableTypeParameter] = []
    for p in program[e].parameters {
      let t = evalTypeAnnotation(p.type)
      result.append(.init(label: p.label?.value, type: t))
    }
    return result
  }

  /// Evaluates and returns the return type annotation of `d`.
  ///
  /// The value of `d`'s explicit return type annotation is returned if it is defined. Otherwise,
  /// a fresh type variable is returned if `d` appears in an expression context (i.e., `d` is the
  /// underlyng declaration of a lambda). Otherwise, `.void` is returned.
  private mutating func evalReturnTypeAnnotation(of d: FunctionDecl.ID) -> AnyType {
    if let o = program[d].output {
      return evalTypeAnnotation(o)
    } else if program[d].isInExprContext {
      return ^freshVariable()
    } else {
      return .void
    }
  }

  /// Evaluates and returns the return type annotation of `d`.
  ///
  /// The value of `d`'s explicit return type annotation is returned if it is defined. Otherwise,
  /// `.void` is returned.
  private mutating func evalReturnTypeAnnotation(of d: MethodDecl.ID) -> AnyType {
    if let o = program[d].output {
      return evalTypeAnnotation(o)
    } else {
      return .void
    }
  }

  /// Evaluates the traits in `composition`, returning a sequence of pairs `(n, t)` where `n` is
  /// an expression in `composition` and `t` the corresponding trait.
  ///
  /// The returned sequence contains an element for each valid trait expression in `composition`.
  /// A diagnostic is reported each invalid expression.
  private mutating func evalTraitComposition(
    _ composition: [NameExpr.ID]
  ) -> [(name: NameExpr.ID, trait: TraitType)] {
    var result: [(name: NameExpr.ID, trait: TraitType)] = []

    for n in composition {
      let t = evalTypeAnnotation(n)
      if let u = TraitType(t) {
        result.append((n, u))
      } else if t[.hasError] {
        // Error already diagnosed.
      } else {
        report(.error(conformanceToNonTraitType: t, at: program[n].site))
      }
    }

    return result
  }

  /// Evaluates and returns `e`, which is a type annotation describing the subject of an extension
  /// or a bound in an existential type, along with its associated constraints.
  ///
  /// When a type annotation describes the subject of an extension or a bound an existential bound,
  /// generic parameters are interpreted as sugared constraints that would otherwise be defined in
  /// a where clause. For example:
  ///
  ///     conformance Array<Int>: P {}
  ///
  /// Here, the extended type is `Array` and `Int` is viewed as a constraint on `Array`.
  private mutating func eval(existentialBound e: AnyExprID) -> (AnyType, Set<GenericConstraint>) {
    let i = evalTypeAnnotation(e)

    // Arguments to bound generic types desugar like where clauses. No constraint is created when
    // an argument refers to its corresponding parameter, unless the `d` is nested in the scope
    // where that parameter is introduced.
    if let b = BoundGenericType(i) {
      for (p, a) in b.arguments {
        assert(GenericTypeParameterType(a as! AnyType)?.decl == p, "not implemented")
      }
      return (b.base, [])
    }

    return (i, [])
  }

  /// Evaluates `e`, which expresses the bounds of an existential type expression, returning the
  /// type of each bound and all associated constraints.
  private mutating func eval(
    existentialInterface e: [NameExpr.ID]
  ) -> (ExistentialType.Interface, Set<GenericConstraint>) {
    guard let (head, tail) = e.headAndTail else { return (.traits([]), []) }

    if tail.isEmpty {
      let (i, cs) = eval(existentialBound: AnyExprID(head))
      if let t = TraitType(i) {
        return (.traits([t]), cs)
      } else {
        return (.generic(i), cs)
      }
    }

    var bounds: Set<TraitType> = []
    var constraints: Set<GenericConstraint> = []

    for n in e {
      let (i, cs) = eval(existentialBound: AnyExprID(n))

      if let u = TraitType(i) {
        bounds.insert(u)
        constraints.formUnion(cs)
      } else if i[.hasError] {
        // Error already diagnosed.
      } else {
        report(.error(notATrait: i, at: program[n].site))
      }
    }

    return (.traits(bounds), constraints)
  }

  // MARK: Name lookup

  /// Returns the declarations that introduce `name` and are exposed to `scopeOfUse`.
  ///
  /// Declarations are lookup up qualified in the declaration space of `nominalScope` if it isn't
  /// `nil`. Otherwise, they are looked up unqualified from `scopeOfuse`.
  private mutating func lookup(
    _ name: SourceRepresentable<Name>, memberOf nominalScope: AnyType?,
    exposedTo scopeOfuse: AnyScopeID
  ) -> [AnyDeclID] {
    var matches = Set<AnyDeclID>()
    if let t = nominalScope {
      matches = lookup(name.value.stem, memberOf: t, exposedTo: scopeOfuse)
    } else {
      matches = lookup(unqualified: name.value.stem, in: scopeOfuse)
    }
    return matches.compactMap({ decl(in: $0, named: name.value) })
  }

  /// Returns the declarations that introduce an entity with given `stem` exposed to `scopeOfUse`
  /// without qualification.
  ///
  /// - Requires: The imports of the module containing `scopeOfUse` have been configured.
  private mutating func lookup(
    unqualified stem: String, in scopeOfUse: AnyScopeID
  ) -> Set<AnyDeclID> {
    var matches = Set<AnyDeclID>()
    var containingFile: TranslationUnit.ID? = nil
    var containingModule: ModuleDecl.ID? = nil

    for s in program.scopes(from: scopeOfUse) {
      if let u = TranslationUnit.ID(s) {
        containingFile = u
      } else if let m = ModuleDecl.ID(s) {
        containingModule = m
      }

      // Gather declarations of the identifier in the current scope; we can assume we've got no
      // non-overloadable candidate.
      let newMatches = lookup(stem, in: s, exposedTo: scopeOfUse)
      for d in newMatches {
        if !insert(lookedUp: d, in: &matches) { return matches }
      }
    }

    // Handle references to the containing module.
    if program[containingModule!].baseName == stem {
      if !insert(lookedUp: AnyDeclID(containingModule!), in: &matches) { return matches }
    }

    // Handle references to imported symbols.
    if let u = containingFile, let fileImports = cache.local.imports[u] {
      for m in fileImports {
        if program[m].baseName == stem {
          matches.insert(AnyDeclID(m))
        } else {
          matches.formUnion(names(introducedIn: m)[stem, default: []])
        }
      }
    }

    return matches
  }

  /// Inserts `d` in `matches` if it isn't shadowed, returning `true` iff name lookup should
  /// continue in outer scopes.
  ///
  /// `d` is inserted if and only if:
  /// - it is not a binding under checking; and
  /// - it `matches` is empty or `d` is overloadable.
  private mutating func insert(lookedUp d: AnyDeclID, in matches: inout Set<AnyDeclID>) -> Bool {
    if (d.kind == VarDecl.self) && cache.declsUnderChecking.contains(d) {
      return true
    } else if d.isOverloadable {
      matches.insert(d)
      return true
    } else if matches.isEmpty {
      matches.insert(d)
      return false
    } else {
      return false
    }
  }

  /// Returns the declarations that introduce a name with given `stem` in the declaration space of
  /// `lookupContext` and are exposed to `scopeOfUse`.
  private mutating func lookup(
    _ stem: String, in lookupContext: AnyScopeID, exposedTo scopeOfUse: AnyScopeID
  ) -> Set<AnyDeclID> {
    switch lookupContext.kind {
    case ProductTypeDecl.self:
      let t = ProductType(NodeID(lookupContext)!, ast: program.ast)
      return lookup(stem, memberOf: ^t, exposedTo: scopeOfUse)
    case TraitDecl.self:
      let t = TraitType(NodeID(lookupContext)!, ast: program.ast)
      return lookup(stem, memberOf: ^t, exposedTo: scopeOfUse)
    case ConformanceDecl.self:
      let d = ConformanceDecl.ID(lookupContext)!
      return lookup(stem, in: d, exposedTo: scopeOfUse)
    case ExtensionDecl.self:
      let d = ExtensionDecl.ID(lookupContext)!
      return lookup(stem, in: d, exposedTo: scopeOfUse)
    case TypeAliasDecl.self:
      let d = TypeAliasDecl.ID(lookupContext)!
      return lookup(stem, in: d, exposedTo: scopeOfUse)
    default:
      return names(introducedIn: lookupContext)[stem, default: []]
    }
  }

  /// Returns the declarations that introduce a name with given `stem` in the declaration space of
  /// `lookupContext` and are exposed to `scopeOfUse`.
  private mutating func lookup<T: TypeExtendingDecl>(
    _ stem: String, in lookupContext: T.ID, exposedTo scopeOfUse: AnyScopeID
  ) -> Set<AnyDeclID> {
    let extended = uncheckedType(of: lookupContext)
    var matches = names(introducedIn: lookupContext)[stem, default: []]
    matches.formUnion(lookup(stem, memberOf: extended, exposedTo: scopeOfUse))
    return matches
  }

  /// Returns the declarations that introduce a name with given `stem` in the declaration space of
  /// `lookupContext` and are exposed to `scopeOfUse`.
  private mutating func lookup(
    _ stem: String, in lookupContext: TypeAliasDecl.ID, exposedTo scopeOfUse: AnyScopeID
  ) -> Set<AnyDeclID> {
    // We can't re-enter `uncheckedType(of:)` if the aliased type of `lookupContext` is being
    // resolved but its generic parameters can be lookep up already.
    if cache.uncheckedType[lookupContext] == .inProgress {
      return names(introducedIn: lookupContext)[stem, default: []]
    }

    let t = uncheckedType(of: lookupContext)
    return lookup(stem, memberOf: t, exposedTo: scopeOfUse)
  }

  /// Returns the declarations that introduce a name with given `stem` as member of `nominalScope`
  /// and are exposed to `scopeOfUse`.
  private mutating func lookup(
    _ stem: String, memberOf nominalScope: AnyType, exposedTo scopeOfUse: AnyScopeID
  ) -> Set<AnyDeclID> {
    switch nominalScope.base {
    case is ErrorType:
      return []
    case let t as BoundGenericType:
      return lookup(stem, memberOf: t.base, exposedTo: scopeOfUse)
    case let t as ConformanceLensType:
      return lookup(stem, memberOf: ^t.lens, exposedTo: scopeOfUse)
    case let t as ExistentialType:
      return lookup(stem, memberOf: t, exposedTo: scopeOfUse)
    case let t as RemoteType:
      return lookup(stem, memberOf: t.bareType, exposedTo: scopeOfUse)
    case let t as GenericTypeParameterType:
      return lookup(stem, memberOf: t, exposedTo: scopeOfUse)
    default:
      break
    }

    let key = Cache.MemberLookupKey(nominalScope, in: scopeOfUse)
    if let m = cache.scopeToMembers[key]?[stem] {
      return m
    }

    var matches: Set<AnyDeclID>
    defer { cache.scopeToMembers[key, default: [:]][stem] = matches }

    switch nominalScope.base {
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
      matches = []
    }

    matches.formUnion(lookup(stem, inExtensionsOf: nominalScope, exposedTo: scopeOfUse))
    return matches
  }

  /// Returns the declarations that introduce a name with given `stem` as member of`nominalScope`
  /// and exposed to `scopeOfUse`.
  private mutating func lookup(
    _ stem: String, memberOf nominalScope: ExistentialType, exposedTo scopeOfUse: AnyScopeID
  ) -> Set<AnyDeclID> {
    // Declarations in extensions of existential types shadow their existential APIs.
    let matches = lookup(stem, inExtensionsOf: ^nominalScope, exposedTo: scopeOfUse)
    if !matches.isEmpty {
      return matches
    }

    // Search in the existential API.
    switch nominalScope.interface {
    case .traits(let s):
      return s.reduce(into: Set<AnyDeclID>()) { (r, t) in
        r.formUnion(lookup(stem, memberOf: ^t, exposedTo: scopeOfUse))
      }
    case .generic(let t):
      return lookup(stem, memberOf: t, exposedTo: scopeOfUse)
    case .metatype:
      return []
    }
  }

  /// Returns the declarations that introduce a name with given `stem` as member of `nominalScope`
  /// and are exposed to `scopeOfUse`.
  private mutating func lookup(
    _ stem: String, memberOf nominalScope: GenericTypeParameterType,
    exposedTo scopeOfUse: AnyScopeID
  ) -> Set<AnyDeclID> {
    var matches = Set<AnyDeclID>()
    for t in conformedTraits(of: nominalScope, in: scopeOfUse) {
      matches.formUnion(lookup(stem, memberOf: ^t, exposedTo: scopeOfUse))
    }
    return matches
  }

  /// Returns the declarations that introduce a name with given `stem` in extensions of
  /// `nominalScope` and are exposed to `scopeOfUse`.
  private mutating func lookup(
    _ stem: String, inExtensionsOf nominalScope: AnyType, exposedTo scopeOfUse: AnyScopeID
  ) -> Set<AnyDeclID> {
    var matches = Set<AnyDeclID>()

    // Look for members declared in extensions.
    for i in extensions(of: nominalScope, exposedTo: scopeOfUse) {
      matches.formUnion(names(introducedIn: AnyScopeID(i)!)[stem, default: []])
    }

    // Look for members declared inherited by conformance/refinement.
    for t in conformedTraits(of: nominalScope, in: scopeOfUse) where nominalScope != t {
      // TODO: Read source of conformance to disambiguate associated names
      let newMatches = lookup(stem, memberOf: ^t, exposedTo: scopeOfUse)

      // Associated type and value declarations are not inherited by conformance. Traits do not
      // inherit the generic parameters.
      switch nominalScope.base {
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

  /// Returns the declarations of an operator specified operator that are visible in `scopeOfUse`.
  func lookup<T: ScopeID>(
    operator operatorName: Identifier, used notation: OperatorNotation,
    in scopeOfUse: T
  ) -> [OperatorDecl.ID] {
    let currentModule = program.module(containing: scopeOfUse)
    if let oper = lookup(operator: operatorName, used: notation, in: currentModule) {
      return [oper]
    }

    return program.ast.modules.compactMap { (m) -> OperatorDecl.ID? in
      if m == currentModule { return nil }
      return lookup(operator: operatorName, used: notation, in: m)
    }
  }

  /// Returns the declaration of the specified operator that are visible in `scopeOfUse`, or `nil`
  /// if no such operator is declared in `scopeOfUse`.
  func lookup(
    operator operatorName: Identifier, used notation: OperatorNotation,
    in scopeOfUse: ModuleDecl.ID
  ) -> OperatorDecl.ID? {
    for decl in program.ast.topLevelDecls(scopeOfUse) where decl.kind == OperatorDecl.self {
      let o = OperatorDecl.ID(decl)!
      if (program[o].notation.value == notation) && (program[o].name.value == operatorName) {
        return o
      }
    }
    return nil
  }

  /// Returns the names introduced in `s`.
  ///
  /// The returned table only contains names whose declaration are lexically contained directly in
  /// `s`. Names introduced by declarations in extensions or inherited by conformance are looked up
  /// using `lookup(_:memberOf:exposedTo:)`.
  private mutating func names<T: ScopeID>(introducedIn s: T) -> Cache.LookupTable {
    // Check if work has to be done.
    if let table = cache.scopeToNames[AnyScopeID(s)] { return table }

    var table: Cache.LookupTable
    defer { cache.scopeToNames[AnyScopeID(s)] = table }

    // Names at module scope are introduced in the module's files.
    if let module = ModuleDecl.ID(s) {
      table = program[module].sources.reduce(into: [:]) { (t, s) in
        t.merge(names(introducedIn: s), uniquingKeysWith: { (l, _) in l })
      }
      return table
    } else {
      table = [:]
    }

    // Nothing to do if the scope contains no declaration.
    let decls = program[AnyScopeID(s)!].decls
    if decls.isEmpty { return [:] }

    for d in decls {
      switch d.kind {
      case FunctionDecl.self:
        guard let i = program[FunctionDecl.ID(d)!].identifier?.value else { continue }
        table[i, default: []].insert(d)
      case InitializerDecl.self:
        table["init", default: []].insert(d)
        table["new", default: []].insert(d)
      case MethodDecl.self:
        table[program[MethodDecl.ID(d)!].identifier.value, default: []].insert(d)
      case SubscriptDecl.self:
        let i = program[SubscriptDecl.ID(d)!].identifier?.value ?? "[]"
        table[i, default: []].insert(d)
      default:
        // Note: operator declarations are not considered during standard name lookup.
        break
      }

      if let e = (program.ast[d] as? SingleEntityDecl) {
        table[e.baseName, default: []].insert(d)
        continue
      }
    }

    return table
  }

  /// Returns the generic parameters introduced by `d`.
  private func genericParameters(introducedBy d: AnyDeclID) -> [GenericParameterDecl.ID] {
    if let g = program.ast[d] as? GenericScope {
      return (d.kind == TraitDecl.self) ? [] : g.genericParameters
    } else {
      return []
    }
  }

  /// Returns declarations extending `subject` exposed to `scopeOfUse`.
  ///
  /// - Requires: The imports of the module containing `scopeOfUse` have been configured.
  /// - Returns: The declarations extending `subject`, which all conform to `TypeExtendingDecl`.
  private mutating func extensions(
    of subject: AnyType, exposedTo scopeOfUse: AnyScopeID
  ) -> [AnyDeclID] {
    let subject = canonical(subject, in: scopeOfUse)
    var matches: [AnyDeclID] = []
    var root: ModuleDecl.ID? = nil

    for s in program.scopes(from: scopeOfUse) {
      switch s.kind {
      case ModuleDecl.self:
        let m = ModuleDecl.ID(s)!
        let symbols = program.ast.topLevelDecls(m)
        reduce(decls: symbols, extending: subject, in: scopeOfUse, into: &matches)
        root = m

      case TranslationUnit.self:
        continue

      default:
        reduce(decls: program[s].decls, extending: subject, in: scopeOfUse, into: &matches)
      }
    }

    // Nowhere else to look if `scopeOfUse` is a module.
    if scopeOfUse.kind == ModuleDecl.self { return matches }

    // Look for extension declarations in imported modules.
    let imports = cache.local.imports[program.source(containing: scopeOfUse), default: []]
    for m in imports where m != root {
      let symbols = program.ast.topLevelDecls(m)
      reduce(decls: symbols, extending: subject, in: scopeOfUse, into: &matches)
    }

    return matches
  }

  /// Insert in `matches` the declarations in `ds` that extend `subject` in `scopeOfUse`.
  ///
  /// - Requires: `subject` must be canonical.
  private mutating func reduce<S: Sequence>(
    decls: S, extending subject: AnyType, in scopeOfUse: AnyScopeID,
    into matches: inout [AnyDeclID]
  ) where S.Element == AnyDeclID {
    precondition(subject[.isCanonical])

    for d in decls where d.kind.value is TypeExtendingDecl.Type {
      // Skip declarations that are already on the checker's stack.
      if cache.uncheckedType[d] == .inProgress { continue }

      // Skip declarations that aren't extending `subject`.
      guard let extended = uncheckedType(of: d).errorFree else { continue }
      if !areEquivalent(extended, subject, in: scopeOfUse) { continue }

      matches.append(d)
    }
  }

  /// Returns `d` if it has name `n`, otherwise the implementation of `d` with name `n` or `nil`
  /// if no such implementation exists.
  ///
  /// - Requires: The base name of `d` is equal to `n.stem`
  private mutating func decl(in d: AnyDeclID, named n: Name) -> AnyDeclID? {
    if !n.labels.isEmpty && (n.labels != labels(d)) { return nil }
    if let x = n.notation, x != operatorNotation(d) { return nil }

    // If the looked up name has an introducer, return the corresponding implementation.
    if let introducer = n.introducer {
      guard let m = program.ast[MethodDecl.ID(d)] else { return nil }
      return m.impls.first(where: { (i) in
        program[i].introducer.value == introducer
      }).map(AnyDeclID.init(_:))
    }

    return d
  }

  /// Returns the labels of `d`s name.
  ///
  /// Only function, method, or subscript declarations may have labels. This method returns `[]`
  /// for any other declaration.
  private mutating func labels(_ d: AnyDeclID) -> [String?] {
    let ast = program.ast
    switch d.kind {
    case FunctionDecl.self:
      return labels(FunctionDecl.ID(d)!)
    case InitializerDecl.self:
      return labels(InitializerDecl.ID(d)!)
    case MethodDecl.self:
      return ast[ast[MethodDecl.ID(d)!].parameters].map(\.label?.value)
    case SubscriptDecl.self:
      return ast[ast[SubscriptDecl.ID(d)!].parameters].map(\.label?.value)
    default:
      return []
    }
  }

  /// Returns the labels of `d`s name.
  private mutating func labels(_ d: FunctionDecl.ID) -> [String?] {
    program.ast[program[FunctionDecl.ID(d)!].parameters].map(\.label?.value)
  }

  /// Returns the labels of `d`s name.
  private mutating func labels(_ d: InitializerDecl.ID) -> [String?] {
    if let t = LambdaType(uncheckedType(of: d)) {
      return Array(t.labels)
    } else if !program[d].isMemberwise {
      return ["self"] + program.ast[program[d].parameters].map(\.label?.value)
    }

    let p = ProductTypeDecl.ID(program[d].scope)!
    return program[p].members.reduce(into: ["self"]) { (l, m) in
      guard let b = BindingDecl.ID(m) else { return }
      for (_, x) in program.ast.names(in: program[b].pattern) {
        l.append(program[x].decl.baseName)
      }
    }
  }

  /// Returns the operator notation of `d`'s name, if any.
  private func operatorNotation(_ d: AnyDeclID) -> OperatorNotation? {
    switch d.kind {
    case FunctionDecl.self:
      return program[FunctionDecl.ID(d)!].notation?.value
    case MethodDecl.self:
      return program[MethodDecl.ID(d)!].notation?.value
    default:
      return nil
    }
  }

  /// If `s` is contained in a type extending declaration, returns the scope extended by that
  /// declaration. Otherwise, returns `nil`.
  private mutating func bridgedScope<S: ScopeID>(of s: S) -> AnyScopeID? {
    switch s.kind {
    case ConformanceDecl.self:
      return scopeExtended(by: ConformanceDecl.ID(s)!)
    case ExtensionDecl.self:
      return scopeExtended(by: ExtensionDecl.ID(s)!)
    case ModuleDecl.self:
      return nil
    default:
      return bridgedScope(of: program[s].scope)
    }
  }

  /// Returns the scope of the declaration extended by `d`, if any.
  mutating func scopeExtended<T: TypeExtendingDecl>(by d: T.ID) -> AnyScopeID? {
    let t = uncheckedType(of: d)
    switch t.base {
    case let u as ProductType:
      return AnyScopeID(u.decl)
    case let u as TypeAliasType:
      return AnyScopeID(u.decl)
    default:
      return nil
    }
  }

  // MARK: Name resolution

  /// Resolves components of `name` from left to right until all components have been resolved or
  /// one component requires overload resolution.
  ///
  /// If the leftmost component of `name` is non-nominal, `resolveNonNominalPrefix` is called on
  /// `self` and the second component `c` of `name` (which is nominal), returning the type `T` of
  /// `c`'s nominal scope or `nil` if such a type can't be determined. If a type is returned, name
  /// resolution proceeds, looking for `c` as a member of `T`. Otherwise, `.canceled(nil, u)` is
  /// returned, where `u` is the nominal suffix of `name`, starting from `c`.
  private mutating func resolve(
    _ name: NameExpr.ID,
    usedAs purpose: NameUse = .unapplied,
    withNonNominalPrefix resolveNonNominalPrefix: (inout Self, NameExpr.ID) -> AnyType?
  ) -> NameResolutionResult {
    var (unresolved, domain) = program.ast.splitNominalComponents(of: name)

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

  /// Resolves a single name component `n` being used as `purpose` in `context`.
  private mutating func resolve(
    _ n: NameExpr.ID, in context: NameResolutionContext?, usedAs purpose: NameUse
  ) -> [NameResolutionResult.Candidate] {
    let name = program[n].name

    // Evaluate generic arguments.
    let arguments = program[n].arguments.map { (a) -> any CompileTimeValue in
      evalTypeAnnotation(a.value)
    }

    let candidates = resolve(
      name, specializedBy: arguments,
      in: context, exposedTo: program[n].scope, usedAs: purpose)

    // Diagnose undefined symbols.
    if candidates.elements.isEmpty {
      report(.error(undefinedName: name.value, in: context?.type, at: name.site))
      return []
    }

    // Diagnose symbols with no viable candidates.
    if candidates.viable.isEmpty {
      if let c = candidates.elements.uniqueElement {
        report(c.diagnostics.elements)
      } else {
        report(.error(noViableCandidateToResolve: name, notes: []))
      }
      return []
    }

    return candidates.viable.map({ candidates.elements[$0] })
  }

  /// Returns the declarations of `name` exposed to `scopeOfUse` and specialized by `arguments`.
  ///
  /// The declarations are searched with an unqualified lookup unless `context` is set, in which
  /// case they are searched in the declaration space of `context.type`. Generic candidates are
  /// specialized with `arguments` appended to `parent.arguments`.
  mutating func resolve(
    _ name: SourceRepresentable<Name>, specializedBy arguments: [any CompileTimeValue],
    in context: NameResolutionContext?, exposedTo scopeOfUse: AnyScopeID, usedAs purpose: NameUse
  ) -> NameResolutionResult.CandidateSet {
    // Resolve references to the built-in symbols.
    if context?.type == .builtin(.module) {
      return resolve(builtin: name)
    }

    // Gather declarations qualified by `parent` if it isn't `nil` or unqualified otherwise.
    let matches = lookup(name, memberOf: context?.type, exposedTo: scopeOfUse)

    // Resolve compilerKnown type aliases if no match was found.
    if matches.isEmpty {
      if context == nil {
        return resolve(compilerKnownAlias: name, specializedBy: arguments, exposedTo: scopeOfUse)
      } else {
        return []
      }
    }

    // Create declaration references to all candidates.
    var candidates: NameResolutionResult.CandidateSet = []
    for m in matches {
      guard var candidateType = resolveType(of: m) else { continue }
      var log = DiagnosticSet()

      // Keep track of generic arguments that should be captured later on.
      let candidateSpecialization = genericArguments(
        passedTo: m, typed: candidateType, referredToBy: name, specializedBy: arguments,
        reportingDiagnosticsTo: &log)

      // If the name resolves to an initializer, determine if it is used as a constructor.
      let isConstructor =
        (m.kind == InitializerDecl.self)
        && ((purpose == .constructor) || (name.value.stem == "new"))
      if isConstructor {
        candidateType = ^LambdaType(constructorFormOf: LambdaType(candidateType)!)
      }

      // If the receiver is an existential, replace its receiver.
      if let t = ExistentialType(context?.type) {
        candidateType = candidateType.asMember(of: t)
      }

      // If the match is introduced in a trait, specialize its receiver as necessary.
      var specialization = context?.arguments ?? [:]
      if let concept = TraitDecl.ID(program.scopeIntroducing(m)), let model = context?.type {
        specialization[program[concept].receiver] = model
      }

      specialization.append(candidateSpecialization)
      candidateType = specialize(candidateType, for: specialization, in: scopeOfUse)

      let r = program.makeReference(
        to: m, specializedBy: specialization, memberOf: context, exposedTo: scopeOfUse,
        usedAsConstructor: isConstructor)

      if let sugars = resolve(
        sugared: name,
        memberOf: .init(type: candidateType, arguments: specialization, receiver: .elided(r)),
        exposedTo: scopeOfUse, usedAs: purpose)
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
      return [.init(f, makingFreshVariableWith: { freshVariable() })]
    }
    if let t = BuiltinType(name.value.stem) {
      return [.init(t)]
    }
    return []
  }

  /// Returns the declarations of `name` interpreted as a compiler-known type alias (e.g., `Never`
  /// or `Union<A, B>`) specialized by `arguments`, or `nil` if an error occured.
  private mutating func resolve(
    compilerKnownAlias name: SourceRepresentable<Name>,
    specializedBy arguments: [any CompileTimeValue],
    exposedTo scopeOfUse: AnyScopeID
  ) -> NameResolutionResult.CandidateSet {
    func nonGeneric(_ t: MetatypeType) -> NameResolutionResult.CandidateSet {
      if arguments.count > 0 {
        report(.error(argumentToNonGenericType: t.instance, at: name.site))
      }
      return [.compilerKnown(^t)]
    }

    // TODO: Check labels and notations.
    switch name.value.stem {
    case "Any":
      return nonGeneric(MetatypeType(of: .any))

    case "Never":
      return nonGeneric(MetatypeType(of: .never))

    case "Builtin":
      let b = program[program.module(containing: scopeOfUse)].canAccessBuiltins
      return b ? [.builtinModule] : []

    case "Union":
      return resolve(union: name, specializedBy: arguments)

    case "Self":
      guard let t = resolveReceiverMetatype(in: scopeOfUse) else { return [] }
      return nonGeneric(t)

    case "Metatype":
      return resolve(metatype: name, specializedBy: arguments)

    default:
      return []
    }
  }

  /// Resolves `name` as a reference to a union type specialized by `arguments`.
  private mutating func resolve(
    union name: SourceRepresentable<Name>, specializedBy arguments: [any CompileTimeValue]
  ) -> NameResolutionResult.CandidateSet {
    var elements: [AnyType] = []
    for a in arguments {
      guard let t = a as? AnyType else {
        report(.error(valueInUnionTypeAt: name.site))
        return [.compilerKnown(.error)]
      }
      elements.append(t)
    }

    switch arguments.count {
    case 0:
      report(.warning(unionTypeWithZeroElementsAt: name.site))
      return [.compilerKnown(^MetatypeType(of: .never))]
    case 1:
      report(.error(unionTypeWithOneElementAt: name.site))
      return [.compilerKnown(.error)]
    default:
      return [.compilerKnown(^MetatypeType(of: UnionType(elements)))]
    }
  }

  /// Resolves `name` as a reference to a metatype specialized by `arguments`.
  private mutating func resolve(
    metatype name: SourceRepresentable<Name>, specializedBy arguments: [any CompileTimeValue]
  ) -> NameResolutionResult.CandidateSet {
    if let a = arguments.uniqueElement {
      let instance = (a as? AnyType) ?? fatalError("not implemented")
      return [.compilerKnown(^MetatypeType(of: MetatypeType(of: instance)))]
    }

    if arguments.isEmpty {
      return [.compilerKnown(^MetatypeType(of: MetatypeType(of: freshVariable())))]
    }

    report(.error(invalidGenericArgumentCountTo: name, found: arguments.count, expected: 1))
    return [.compilerKnown(.error)]
  }

  /// Resolves `name` as a sugared reference to a constructor or nameless subscript declaration or
  /// returns `nil` if `name` isn't a sugar.
  private mutating func resolve(
    sugared name: SourceRepresentable<Name>, memberOf parent: NameResolutionContext,
    exposedTo scopeOfUse: AnyScopeID, usedAs purpose: NameUse
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
        n, specializedBy: [],
        in: .init(type: t, arguments: parent.arguments, receiver: nil),
        exposedTo: scopeOfUse, usedAs: .constructor)
      return r.elements.isEmpty ? nil : r

    case .subscript where !(parent.type.base is MetatypeType):
      let n = SourceRepresentable(value: Name(stem: "[]"), range: name.site)
      let r = resolve(
        n, specializedBy: [],
        in: parent,
        exposedTo: scopeOfUse, usedAs: .subscript)
      return r.elements.isEmpty ? nil : r

    default:
      return nil
    }
  }

  /// Returns the resolved type of the entity declared by `d` or `nil` if is invalid.
  private mutating func resolveType(of d: AnyDeclID) -> AnyType? {
    var result = uncheckedType(of: d)
    if result.isError { return nil }

    // Properties are not first-class.
    if let s = SubscriptDecl.ID(d), program[s].isProperty {
      result = SubscriptType(result)!.output
    }

    // Erase parameter conventions.
    if let t = ParameterType(result) {
      result = t.bareType
    }

    return result
  }

  /// Computes and returns the type of `Self` in `scopeOfUse`.
  private mutating func resolveReceiverMetatype(in scopeOfUse: AnyScopeID) -> MetatypeType? {
    switch scopeOfUse.kind {
    case ConformanceDecl.self:
      return resolveReceiverMetatype(in: ConformanceDecl.ID(scopeOfUse)!)
    case ExtensionDecl.self:
      return resolveReceiverMetatype(in: ExtensionDecl.ID(scopeOfUse)!)
    case ProductTypeDecl.self:
      return resolveReceiverMetatype(in: ProductTypeDecl.ID(scopeOfUse)!)
    case TraitDecl.self:
      return resolveReceiverMetatype(in: TraitDecl.ID(scopeOfUse)!)
    case TypeAliasDecl.self:
      return resolveReceiverMetatype(in: TypeAliasDecl.ID(scopeOfUse)!)
    case ModuleDecl.self:
      return nil
    default:
      return resolveReceiverMetatype(in: program[scopeOfUse].scope)
    }
  }

  /// Computes and returns the type of `Self` in `scopeOfUse`.
  private mutating func resolveReceiverMetatype<T: GenericDecl>(
    in scopeOfUse: T.ID
  ) -> MetatypeType? {
    guard let unspecialized = MetatypeType(uncheckedType(of: scopeOfUse)) else {
      return nil
    }

    guard let parameters = program[scopeOfUse].genericClause?.value.parameters else {
      return unspecialized
    }

    let arguments = GenericArguments(
      uniqueKeysWithValues: parameters.map({ (p) in
        (key: p, value: ^GenericTypeParameterType(p, ast: program.ast))
      }))
    return MetatypeType(of: BoundGenericType(unspecialized.instance, arguments: arguments))
  }

  /// Computes and returns the type of `Self` in `scopeOfUse`.
  private mutating func resolveReceiverMetatype<T: TypeExtendingDecl>(
    in scopeOfUse: T.ID
  ) -> MetatypeType? {
    let t = uncheckedType(of: scopeOfUse)

    switch t.base {
    case let u as ProductType:
      return resolveReceiverMetatype(in: u.decl)
    case let u as TypeAliasType:
      return resolveReceiverMetatype(in: u.decl)
    default:
      return MetatypeType(of: t)
    }
  }

  /// Computes and returns the type of `Self` in `scopeOfUse`.
  private mutating func resolveReceiverMetatype(in scopeOfUse: TraitDecl.ID) -> MetatypeType? {
    MetatypeType(of: GenericTypeParameterType(selfParameterOf: scopeOfUse, in: program.ast))
  }

  /// Resolves `c`, which occurs in `d`, returning its name and declaration or `nil` if either `c`
  /// doesn't have to be captured or name resolution failed.
  private mutating func resolveImplicitCapture<T: Decl & LexicalScope>(
    _ c: NameExpr.ID, occuringIn d: T.ID
  ) -> (stem: String, decl: AnyDeclID)? {
    let n = program[c].name
    var candidates = lookup(unqualified: n.value.stem, in: program[c].scope)
    candidates.removeAll(where: { isCaptured(referenceTo: $0, occuringIn: AnyScopeID(d)) })
    if candidates.isEmpty { return nil }

    guard let pick = candidates.uniqueElement else {
      report(.error(cannotCaptureOverloadedNameImplicitly: n))
      return nil
    }

    if program.isMember(pick) {
      return ("self", lookup(unqualified: "self", in: AnyScopeID(d)).uniqueElement!)
    } else {
      return (n.value.stem, pick)
    }
  }

  /// Returns `true` if references to `d` are captured if they occur in `scopeOfUse`.
  private mutating func isCaptured(
    referenceTo d: AnyDeclID, occuringIn scopeOfUse: AnyScopeID
  ) -> Bool {
    if program.isContained(program[d].scope, in: scopeOfUse) { return false }
    if program.isGlobal(d) { return false }
    if program.isMember(d) {
      let lhs = resolveReceiverMetatype(in: program[d].scope)
      let rhs = resolveReceiverMetatype(in: program[scopeOfUse].scope)
      if lhs != rhs {
        return false
      }
    }

    // Capture-less functions are not captured.
    if d.kind == FunctionDecl.self {
      return areEquivalent(
        LambdaType(uncheckedType(of: d))?.environment ?? .error, .void, in: scopeOfUse)
    } else {
      return true
    }
  }

  /// Returns the list of generic arguments passed to `d`, which has type `t` and is being referred
  /// to by `name`, reporting diagnostics to `log.`
  private mutating func genericArguments(
    passedTo d: AnyDeclID, typed t: AnyType,
    referredToBy name: SourceRepresentable<Name>, specializedBy arguments: [any CompileTimeValue],
    reportingDiagnosticsTo log: inout DiagnosticSet
  ) -> GenericArguments {
    if let g = BoundGenericType(t) {
      assert(arguments.isEmpty, "generic declaration bound twice")
      return g.arguments
    } else {
      let p = genericParameters(introducedBy: d)
      return associateGenericParameters(p, of: name, to: arguments, reportingDiagnosticsTo: &log)
    }
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
      result[p] = ^GenericTypeParameterType(p, ast: program.ast)
    }

    if !arguments.isEmpty && (parameters.count != arguments.count) {
      let (f, e) = (arguments.count, parameters.count)
      log.insert(.error(invalidGenericArgumentCountTo: name, found: f, expected: e))
    }

    return result
  }

  // MARK: Quantifier elimination

  /// A context in which a generic parameter can be instantiated.
  private struct InstantiationContext {

    /// The scope in which the parameter is being used.
    let scopeOfUse: AnyScopeID

    /// The scope extended by `scopeOfUse` if the latter is contained in an extension.
    let extendedScope: AnyScopeID?

  }

  /// Creates a context for instantiating generic parameters used in `scopeOfUse`.
  private mutating func instantiationContext(in scopeOfUse: AnyScopeID) -> InstantiationContext {
    .init(scopeOfUse: AnyScopeID(scopeOfUse), extendedScope: bridgedScope(of: scopeOfUse))
  }

  /// Creates a context for instantiating generic parameters used in `scopeOfUse` by a name
  /// referring to `r`.
  private mutating func instantiationContext(
    _ r: DeclReference, in scopeOfUse: AnyScopeID
  ) -> InstantiationContext {
    // References to constructors are always opened.
    if case .constructor(let d, _) = r {
      return instantiationContext(in: program[program[d].scope].scope)
    }

    if let d = r.decl, isRecursive(useOf: d, in: scopeOfUse) {
      return instantiationContext(in: program[d].scope)
    } else {
      return instantiationContext(in: scopeOfUse)
    }
  }

  /// Replaces the generic parameters in `subject` by fresh variables, forming instantiation
  /// constraints at `site`.
  private mutating func open(type: AnyType, at site: SourceRange) -> InstantiatedType {
    // Since no generic parameter can be introduced at module scope, passing a module here will
    // force all parameters to be opened.
    instantiate(
      type, in: AnyScopeID(program.ast.coreLibrary!), cause: .init(.structural, at: site))
  }

  /// Replaces the generic parameters in `candidate` by fresh variables if their environments don't
  /// contain `scopeOfUse`, updating `substitutions` with opened generic parameters and anchoring
  /// instantiation constraints at `cause`.
  mutating func instantiate(
    _ candidate: NameResolutionResult.Candidate, in scopeOfUse: AnyScopeID,
    updating substitutions: inout [GenericParameterDecl.ID: AnyType],
    anchoringInstantiationConstraintsAt cause: ConstraintOrigin
  ) -> NameResolutionResult.Candidate {
    let ctx = instantiationContext(candidate.reference, in: scopeOfUse)

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

  /// Replaces the generic parameters in `subject` by fresh variables if their environments don't
  /// contain `scopeOfUse`, assigning `cause` to instantiation constraints.
  private mutating func instantiate(
    _ subject: AnyType, in scopeOfUse: AnyScopeID, cause: ConstraintOrigin
  ) -> InstantiatedType {
    let ctx = instantiationContext(in: scopeOfUse)
    var substitutions: [GenericParameterDecl.ID: AnyType] = [:]
    return instantiate(subject, in: ctx, cause: cause, updating: &substitutions)
  }

  /// Replaces the generic parameters in `subject` by fresh variables if their environments don't
  /// contain `contextOfUse`, assigning `cause` to instantiation constraints.
  private mutating func instantiate(
    _ subject: AnyType, in contextOfUse: InstantiationContext, cause: ConstraintOrigin,
    updating substitutions: inout [GenericParameterDecl.ID: AnyType]
  ) -> InstantiatedType {
    func instantiate(mutating me: inout Self, type: AnyType) -> TypeTransformAction {
      switch type.base {
      case is AssociatedTypeType:
        fatalError("not implemented")

      case let p as GenericTypeParameterType:
        if let t = substitutions[p.decl] {
          return .stepOver(t)
        } else if me.shouldOpen(p, in: contextOfUse) {
          // TODO: Collect constraints
          return .stepOver(substitutions[p.decl].setIfNil(^me.freshVariable()))
        } else {
          return .stepOver(substitutions[p.decl].setIfNil(type))
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

    let shape = subject.transform(mutating: &self, instantiate(mutating:type:))
    return InstantiatedType(shape: shape, constraints: [])
  }

  /// Returns `true` iff `contextOfUse` is not contained in `p`'s environment.
  private func shouldOpen(
    _ p: GenericTypeParameterType, in contextOfUse: InstantiationContext
  ) -> Bool {
    let introductionScope = program[p.decl].scope

    if program.isContained(contextOfUse.scopeOfUse, in: introductionScope) {
      return false
    } else if let s = contextOfUse.extendedScope {
      return !program.isContained(s, in: introductionScope)
    } else {
      return true
    }
  }

  /// Returns `true` iff a use of `d` in `scopeOfUse` is recursive.
  private func isRecursive(useOf d: AnyDeclID, in scopeOfUse: AnyScopeID) -> Bool {
    if let s = AnyScopeID(d) {
      return d.isCallable && program.isContained(scopeOfUse, in: s)
    } else {
      return false
    }
  }

  // MARK: Type inference

  /// Returns the inferred type of `d`, inserting new proof obligations in `obligations`.
  ///
  /// `isConditional` is `true` iff `d` is used for pattern matching.
  private mutating func inferredType(
    of d: BindingDecl.ID, usedAsCondition isConditional: Bool,
    updating obligations: inout ProofObligations
  ) -> AnyType {
    guard let pattern = inferredType(of: program[d].pattern, updating: &obligations).errorFree
    else { return .error }

    guard let i = program[d].initializer else {
      assert(program[d].pattern.annotation != nil, "expected type annotation")
      return pattern
    }

    // Note: `i` should not have been assigned a type if before `d` is checked.
    assert(cache.local.exprType[i] == nil)
    let initializer = constrain(i, to: ^freshVariable(), in: &obligations)

    // If `d` has no annotation, its type is inferred as that of its initializer. Otherwise, the
    // type of the initializer must be subtype of the pattern unless `d` is used as a condition.
    // In that case, it must be supertype of the pattern.
    if program[d].pattern.annotation == nil {
      let o = ConstraintOrigin(.initializationWithPattern, at: program[i].site)
      obligations.insert(EqualityConstraint(initializer, pattern, origin: o))
    } else if !isConditional {
      let o = ConstraintOrigin(.initializationWithHint, at: program[i].site)
      obligations.insert(SubtypingConstraint(initializer, pattern, origin: o))
    } else {
      let o = ConstraintOrigin(.optionalBinding, at: program[i].site)
      obligations.insert(SubtypingConstraint(pattern, initializer, origin: o))
    }

    // Collect the proof obligations for the initializer. The resulting type can be discarded; the
    // type of `i` will be assigned after the all proof obligations are discharged.
    _ = inferredType(of: i, withHint: pattern, updating: &obligations)
    return pattern
  }

  /// Returns the inferred type of `e`, updating `obligations` and gathering contextual information
  /// from `hint`.
  private mutating func inferredType<T: ExprID>(
    of e: T, withHint hint: AnyType? = nil,
    updating obligations: inout ProofObligations
  ) -> AnyType {
    // Preserve choices already committed to the AST.
    if let t = cache.local.exprType[e] { return t }
    defer { assert(obligations.exprType[e] != nil) }

    switch e.kind {
    case BooleanLiteralExpr.self:
      return _inferredType(of: BooleanLiteralExpr.ID(e)!, withHint: hint, updating: &obligations)
    case CastExpr.self:
      return _inferredType(of: CastExpr.ID(e)!, withHint: hint, updating: &obligations)
    case ConditionalExpr.self:
      return _inferredType(of: ConditionalExpr.ID(e)!, withHint: hint, updating: &obligations)
    case ExistentialTypeExpr.self:
      return _inferredType(of: ExistentialTypeExpr.ID(e)!, withHint: hint, updating: &obligations)
    case FloatLiteralExpr.self:
      return _inferredType(of: FloatLiteralExpr.ID(e)!, withHint: hint, updating: &obligations)
    case FunctionCallExpr.self:
      return _inferredType(of: FunctionCallExpr.ID(e)!, withHint: hint, updating: &obligations)
    case InoutExpr.self:
      return _inferredType(of: InoutExpr.ID(e)!, withHint: hint, updating: &obligations)
    case IntegerLiteralExpr.self:
      return _inferredType(of: IntegerLiteralExpr.ID(e)!, withHint: hint, updating: &obligations)
    case LambdaExpr.self:
      return _inferredType(of: LambdaExpr.ID(e)!, withHint: hint, updating: &obligations)
    case MatchExpr.self:
      return _inferredType(of: MatchExpr.ID(e)!, withHint: hint, updating: &obligations)
    case NameExpr.self:
      return _inferredType(of: NameExpr.ID(e)!, withHint: hint, updating: &obligations)
    case PragmaLiteralExpr.self:
      return _inferredType(of: PragmaLiteralExpr.ID(e)!, withHint: hint, updating: &obligations)
    case RemoteExpr.self:
      return _inferredType(of: RemoteExpr.ID(e)!, withHint: hint, updating: &obligations)
    case SequenceExpr.self:
      return _inferredType(of: SequenceExpr.ID(e)!, withHint: hint, updating: &obligations)
    case StringLiteralExpr.self:
      return _inferredType(of: StringLiteralExpr.ID(e)!, withHint: hint, updating: &obligations)
    case SubscriptCallExpr.self:
      return _inferredType(of: SubscriptCallExpr.ID(e)!, withHint: hint, updating: &obligations)
    case TupleExpr.self:
      return _inferredType(of: TupleExpr.ID(e)!, withHint: hint, updating: &obligations)
    case TupleMemberExpr.self:
      return _inferredType(of: TupleMemberExpr.ID(e)!, withHint: hint, updating: &obligations)
    default:
      unexpected(e, in: program.ast)
    }
  }

  /// Returns the inferred type of `e`, updating `obligations` and gathering contextual information
  /// from `hint`.
  private mutating func _inferredType(
    of e: BooleanLiteralExpr.ID, withHint hint: AnyType? = nil,
    updating obligations: inout ProofObligations
  ) -> AnyType {
    constrain(e, to: ^program.ast.coreType("Bool")!, in: &obligations)
  }

  /// Returns the inferred type of `e`, updating `obligations` and gathering contextual information
  /// from `hint`.
  private mutating func _inferredType(
    of e: CastExpr.ID, withHint hint: AnyType? = nil,
    updating obligations: inout ProofObligations
  ) -> AnyType {
    guard let target = evalTypeAnnotation(program[e].right).errorFree else {
      return constrain(e, to: .error, in: &obligations)
    }

    let cause = ConstraintOrigin(.cast, at: program[e].site)

    let rhs = instantiate(target, in: program[e].scope, cause: cause)
    constrain(program[e].right, to: ^MetatypeType(of: rhs.shape), in: &obligations)
    obligations.insert(rhs.constraints)

    switch program[e].direction {
    case .down:
      // Note: constraining the type of the left operand to be above the right operand wouldn't
      // contribute any useful information to the constraint system.
      _ = inferredType(of: program[e].left, updating: &obligations)

    case .up:
      // The type of the left operand must be statically known to subtype of the right operand.
      let lhs = inferredType(
        of: program[e].left, withHint: ^freshVariable(), updating: &obligations)
      obligations.insert(SubtypingConstraint(lhs, rhs.shape, origin: cause))

    case .pointerConversion:
      // The left operand must be a `Builtin.ptr`. The right operand must be a remote type.
      if !(rhs.shape.base is RemoteType) {
        report(.error(invalidPointerConversionAt: program[e].right.site))
        return constrain(e, to: .error, in: &obligations)
      }

      let lhs = inferredType(of: program[e].left, updating: &obligations)
      obligations.insert(EqualityConstraint(lhs, .builtin(.ptr), origin: cause))
    }

    // Unless an error occurred, the inferred type is `rhs`.
    return constrain(e, to: rhs.shape, in: &obligations)
  }

  /// Returns the inferred type of `e`, updating `obligations` and gathering contextual information
  /// from `hint`.
  private mutating func _inferredType(
    of e: ConditionalExpr.ID, withHint hint: AnyType? = nil,
    updating obligations: inout ProofObligations
  ) -> AnyType {
    // Condition doesn't participate in inference.
    check(program[e].condition)

    let a = inferredType(of: program[e].success, withHint: hint, updating: &obligations)
    let b = inferredType(of: program[e].failure, withHint: hint, updating: &obligations)
    let t = ^freshVariable()
    obligations.insert(
      MergingConstraint(t, [a, b], origin: .init(.branchMerge, at: program[e].introducerSite)))
    return constrain(e, to: t, in: &obligations)
  }

  /// Returns the inferred type of `e`, updating `obligations` and gathering contextual information
  /// from `hint`.
  private mutating func _inferredType(
    of e: ExistentialTypeExpr.ID, withHint hint: AnyType? = nil,
    updating obligations: inout ProofObligations
  ) -> AnyType {
    if let t = evalTypeAnnotation(e).errorFree {
      return constrain(e, to: ^MetatypeType(of: t), in: &obligations)
    } else {
      return constrain(e, to: .error, in: &obligations)
    }
  }

  /// Returns the inferred type of `e`, updating `obligations` and gathering contextual information
  /// from `hint`.
  private mutating func _inferredType(
    of e: FloatLiteralExpr.ID, withHint hint: AnyType? = nil,
    updating obligations: inout ProofObligations
  ) -> AnyType {
    let t = ^program.ast.coreType("Float64")!
    return _inferredType(ofLiteral: e, withHint: hint, defaultingTo: t, updating: &obligations)
  }

  /// Returns the inferred type of `e`, updating `obligations` and gathering contextual information
  /// from `hint`.
  private mutating func _inferredType(
    of e: FunctionCallExpr.ID, withHint hint: AnyType? = nil,
    updating obligations: inout ProofObligations
  ) -> AnyType {
    let callee = _inferredType(
      ofCallee: program[e].callee, usedAs: .function, withHint: hint,
      appliedTo: program[e].arguments, updating: &obligations)

    // We failed to infer the type of the callee. We can stop here.
    if callee.isError {
      return constrain(e, to: .error, in: &obligations)
    }

    // The callee has a callable type or we need inference to determine its type. Either way,
    // constraint the callee and its arguments with a function call constraints.
    if isArrow(callee) || (callee.base is TypeVariable) {
      return _inferredType(of: e, withCallee: callee, withHint: hint, updating: &obligations)
    }

    // In any other case, the callee is known to be not callable.
    report(.error(cannotCall: callee, as: .function, at: program[e].callee.site))
    return constrain(e, to: .error, in: &obligations)
  }

  /// Returns the inferred type of `e` knowing its callee has type `callee`, updating `obligations`
  /// and gathering contextual information from `hint`.
  private mutating func _inferredType(
    of e: FunctionCallExpr.ID, withCallee callee: AnyType, withHint hint: AnyType?,
    updating obligations: inout ProofObligations
  ) -> AnyType {
    var arguments: [CallConstraint.Argument] = []
    for a in program[e].arguments {
      let p = inferredType(of: a.value, withHint: ^freshVariable(), updating: &obligations)
      arguments.append(.init(label: a.label, type: p, valueSite: program[a.value].site))
    }

    let output = ((callee.base as? CallableType)?.output ?? hint) ?? ^freshVariable()
    obligations.insert(
      CallConstraint(
        arrow: callee, takes: arguments, gives: output,
        origin: .init(.callee, at: program[e].callee.site)))

    return constrain(e, to: output, in: &obligations)
  }

  /// Returns the inferred type of `e`, updating `obligations` and gathering contextual information
  /// from `hint`.
  private mutating func _inferredType(
    of e: InoutExpr.ID, withHint hint: AnyType? = nil,
    updating obligations: inout ProofObligations
  ) -> AnyType {
    let t = inferredType(of: program[e].subject, withHint: hint, updating: &obligations)
    return constrain(e, to: t, in: &obligations)
  }

  /// Returns the inferred type of `e`, updating `obligations` and gathering contextual information
  /// from `hint`.
  private mutating func _inferredType(
    of e: IntegerLiteralExpr.ID, withHint hint: AnyType? = nil,
    updating obligations: inout ProofObligations
  ) -> AnyType {
    let t = ^program.ast.coreType("Int")!
    return _inferredType(ofLiteral: e, withHint: hint, defaultingTo: t, updating: &obligations)
  }

  /// Returns the inferred type of `e`, updating `obligations` and gathering contextual information
  /// from `hint`.
  private mutating func _inferredType(
    of e: LambdaExpr.ID, withHint hint: AnyType? = nil,
    updating obligations: inout ProofObligations
  ) -> AnyType {
    let h = LambdaType(hint)
    let d = program[e].decl.id
    let inputs = uncheckedInputTypes(of: e, withHint: h)

    let captures = uncheckedCaptureTypes(of: d)
    let environment = TupleType(captures.explict + captures.implicit)

    let output = inferredReturnType(of: e, withHint: h?.output, updating: &obligations)
    if output.isError {
      return constrain(e, to: .error, in: &obligations)
    }

    let effect: AccessEffect
    if let k = program[d].receiverEffect {
      effect = k.value
    } else if program[d].explicitCaptures.contains(where: isVar(_:)) {
      effect = .inout
    } else if captures.implicit.contains(where: { RemoteType($0.type)!.access == .inout }) {
      effect = .inout
    } else {
      effect = .let
    }

    let result = ^LambdaType(
      receiverEffect: effect,
      environment: ^environment,
      inputs: inputs, output: output)
    obligations.assign(result, to: AnyDeclID(d))
    return constrain(e, to: result, in: &obligations)
  }

  /// Returns the inferred type of `e`, updating `obligations` and gathering contextual information
  /// from `hint`.
  private mutating func _inferredType(
    of e: MatchExpr.ID, withHint hint: AnyType? = nil,
    updating obligations: inout ProofObligations
  ) -> AnyType {
    let subject = inferredType(of: program[e].subject, updating: &obligations)
    for c in program[e].cases {
      _ = inferredType(of: program[c].pattern, withHint: subject, updating: &obligations)
    }
    return constrain(e, to: AnyType.void, in: &obligations)
  }

  /// Returns the inferred type of `e`, updating `obligations` and gathering contextual information
  /// from `hint`.
  ///
  /// - Parameters:
  ///   - implicitNominalScope: the type of the nominal scope in which the leftmost component of
  ///     `e` is resolved if it is implicit (e.g., `.foo.bar`).
  ///   - purpose: How `e` is used.
  private mutating func _inferredType(
    of e: NameExpr.ID, inImplicitScope implicitNominalScope: AnyType? = nil,
    usedAs purpose: NameUse = .unapplied,
    withHint hint: AnyType? = nil,
    updating obligations: inout ProofObligations
  ) -> AnyType {
    let resolution = resolve(e, usedAs: purpose) { (me, n) in
      switch me.program[n].domain {
      case .explicit(let e):
        return me.inferredType(of: e, updating: &obligations)
      case .implicit:
        return implicitNominalScope
      case .none, .operand:
        unreachable()
      }
    }

    let unresolved: [NameExpr.ID]
    var lastVisited: AnyType?

    switch resolution {
    case .failed:
      return constrain(e, to: .error, in: &obligations)

    case .canceled(let p, let suffix):
      if p == nil {
        report(.error(noContextToResolve: program[e].name.value, at: program[e].name.site))
        return constrain(e, to: .error, in: &obligations)
      }
      unresolved = suffix
      lastVisited = p

    case .done(let prefix, let suffix):
      unresolved = suffix
      lastVisited = constrain(prefix, in: &obligations)
    }

    // Create the necessary constraints to let the solver resolve the remaining components.
    for (i, component) in unresolved.enumerated() {
      let memberType = ^freshVariable()
      obligations.insert(
        MemberConstraint(
          lastVisited!, hasMember: memberType, referredToBy: component, in: program.ast,
          usedAs: (i == unresolved.count - 1) ? purpose : .unapplied,
          origin: ConstraintOrigin(.member, at: program[component].site)))
      lastVisited = constrain(component, to: memberType, in: &obligations)
    }

    return lastVisited!
  }

  /// Returns the inferred type of `e`, updating `obligations` and gathering contextual information
  /// from `hint`.
  private mutating func _inferredType(
    of e: PragmaLiteralExpr.ID, withHint hint: AnyType? = nil,
    updating obligations: inout ProofObligations
  ) -> AnyType {
    switch program.ast[e].kind {
    case .file:
      return constrain(e, to: ^program.ast.coreType("String")!, in: &obligations)
    case .line:
      return constrain(e, to: ^program.ast.coreType("Int")!, in: &obligations)
    }
  }

  /// Returns the inferred type of `e`, using `hint` for context and updating `obligations`.
  private mutating func _inferredType(
    of e: RemoteExpr.ID, withHint hint: AnyType? = nil,
    updating obligations: inout ProofObligations
  ) -> AnyType {
    let t = inferredType(
      of: program[e].operand, withHint: RemoteType(hint)?.bareType, updating: &obligations)
    return constrain(e, to: ^RemoteType(program[e].convention.value, t), in: &obligations)
  }

  /// Returns the inferred type of `e`, updating `obligations` and gathering contextual information
  /// from `hint`.
  private mutating func _inferredType(
    of e: SequenceExpr.ID, withHint hint: AnyType? = nil,
    updating obligations: inout ProofObligations
  ) -> AnyType {
    // Transform the sequence into a binary tree.
    guard let tree = fold(e) else {
      return constrain(e, to: .error, in: &obligations)
    }

    // Generate constraints from the folded sequence.
    let root = _inferredType(of: tree, withHint: hint, updating: &obligations)
    return constrain(e, to: root, in: &obligations)
  }

  /// Returns the inferred type of `e`, updating `obligations` and gathering contextual information
  /// from `hint`.
  private mutating func _inferredType(
    of e: FoldedSequenceExpr, withHint hint: AnyType? = nil,
    updating obligations: inout ProofObligations
  ) -> AnyType {
    switch e {
    case .infix(let callee, let lhs, let rhs):
      // Infer the types of the operands.
      let lhsType = _inferredType(of: lhs, updating: &obligations)
      let rhsType = _inferredType(of: rhs, updating: &obligations)

      if lhsType.isError || rhsType.isError {
        return .error
      }

      let operatorType = ^freshVariable()
      let outputType = ^freshVariable()
      constrain(callee.expr, to: operatorType, in: &obligations)

      // The operator is a member function of the left operand.
      obligations.insert(
        MemberConstraint(
          lhsType, hasMember: operatorType, referredToBy: callee.expr, in: program.ast,
          usedAs: .unapplied,
          origin: .init(.member, at: program[callee.expr].site)))
      obligations.insert(
        CallConstraint(
          arrow: operatorType,
          takes: [.init(label: nil, type: rhsType, valueSite: program.ast.site(of: rhs))],
          gives: outputType,
          origin: .init(.callee, at: program.ast.site(of: e))))

      return outputType

    case .leaf(let l):
      return inferredType(of: l, withHint: hint, updating: &obligations)
    }
  }

  /// Returns the inferred type of `e`, updating `obligations` and gathering contextual information
  /// from `hint`.
  private mutating func _inferredType(
    of e: StringLiteralExpr.ID, withHint hint: AnyType? = nil,
    updating obligations: inout ProofObligations
  ) -> AnyType {
    constrain(e, to: ^program.ast.coreType("String")!, in: &obligations)
  }

  /// Returns the inferred type of `e`, updating `obligations` and gathering contextual information
  /// from `hint`.
  private mutating func _inferredType(
    of e: SubscriptCallExpr.ID, withHint hint: AnyType? = nil,
    updating obligations: inout ProofObligations
  ) -> AnyType {
    let callee = _inferredType(
      ofCallee: program[e].callee, usedAs: .subscript, withHint: hint,
      appliedTo: program[e].arguments, updating: &obligations)

    // We failed to infer the type of the callee. We can stop here.
    if callee.isError {
      return constrain(e, to: .error, in: &obligations)
    }

    // The callee has a metatype and is a name expression bound to a nominal type declaration,
    // meaning that the call is actually a sugared buffer type expression.
    if isBoundToNominalTypeDecl(program[e].callee, in: obligations) {
      fatalError("not implemented")
    }

    // The callee has a callable type or we need inference to determine its type. Either way,
    // constraint the callee and its arguments with a function call constraints.
    if isSubscript(callee) || (callee.base is TypeVariable) {
      return _inferredType(of: e, withCallee: callee, withHint: hint, updating: &obligations)
    }

    // In any other case, the callee is known to be not callable.
    report(.error(cannotCall: callee, as: .subscript, at: program[e].callee.site))
    return constrain(e, to: .error, in: &obligations)
  }

  /// Returns the inferred type of `e` knowing its callee has type `callee`, updating `obligations`
  /// and gathering contextual information from `hint`.
  private mutating func _inferredType(
    of e: SubscriptCallExpr.ID, withCallee callee: AnyType, withHint hint: AnyType?,
    updating obligations: inout ProofObligations
  ) -> AnyType {
    var arguments: [CallConstraint.Argument] = []
    for a in program[e].arguments {
      let p = inferredType(of: a.value, withHint: ^freshVariable(), updating: &obligations)
      arguments.append(.init(label: a.label, type: p, valueSite: program[a.value].site))
    }

    let output = ((callee.base as? CallableType)?.output ?? hint) ?? ^freshVariable()
    obligations.insert(
      CallConstraint(
        subscript: callee, takes: arguments, gives: output,
        origin: .init(.callee, at: program[e].callee.site)))

    return constrain(e, to: output, in: &obligations)
  }

  /// Returns the inferred type of `e`, updating `obligations` and gathering contextual information
  /// from `hint`.
  private mutating func _inferredType(
    of e: TupleExpr.ID, withHint hint: AnyType? = nil,
    updating obligations: inout ProofObligations
  ) -> AnyType {
    let elements = program[e].elements
    var elementTypes: [TupleType.Element] = []

    // If the expected type is a tuple compatible with the shape of the expression, propagate that
    // information down the expression tree. Otherwise, infer the type of the expression from the
    // leaves and use type constraints to detect potential mismatch.
    if let type = TupleType(hint),
      type.elements.elementsEqual(elements, by: { (a, b) in a.label == b.label?.value })
    {
      for (t, e) in zip(type.elements, elements) {
        let u = inferredType(of: e.value, withHint: t.type, updating: &obligations)
        elementTypes.append(.init(label: e.label?.value, type: u))
      }
    } else {
      for e in elements {
        let u = inferredType(of: e.value, withHint: nil, updating: &obligations)
        elementTypes.append(.init(label: e.label?.value, type: u))
      }
    }

    return constrain(e, to: ^TupleType(elementTypes), in: &obligations)
  }

  /// Returns the inferred type of `e`, updating `obligations` and gathering contextual information
  /// from `hint`.
  private mutating func _inferredType(
    of e: TupleMemberExpr.ID, withHint hint: AnyType? = nil,
    updating obligations: inout ProofObligations
  ) -> AnyType {
    let s = inferredType(of: program[e].tuple, withHint: nil, updating: &obligations)
    let t = ^freshVariable()
    let i = program[e].index
    obligations.insert(
      TupleMemberConstraint(s, at: i.value, hasType: t, origin: .init(.member, at: i.site)))
    return constrain(e, to: t, in: &obligations)
  }

  /// Returns the inferred type of `callee`, which is the callee of a function, initializer, or
  /// subscript applied to with `arguments`, updating `state` with inference facts and deferred
  /// type checking requests.
  private mutating func _inferredType(
    ofCallee callee: AnyExprID, usedAs purpose: NameUse, withHint hint: AnyType?,
    appliedTo arguments: [LabeledArgument],
    updating obligations: inout ProofObligations
  ) -> AnyType {
    assert(purpose != .unapplied)
    switch callee.kind {
    case NameExpr.self:
      let e = NameExpr.ID(callee)!
      return _inferredType(of: e, inImplicitScope: hint, usedAs: purpose, updating: &obligations)
    default:
      return inferredType(of: callee, updating: &obligations)
    }
  }

  /// Returns the inferred type of `literal`, which is a literal, updating `obligations`, gathering
  /// contextual information from `hint`, and defaulting to `defaultType` if contextual information
  /// doesn't allow top-down inference.
  ///
  /// If `hint` is not `nil`, it is constrained to be conforming to the `ExpressibleBy***Literal`
  /// corresponding to `defaultType` and the type of `e` if inferred as `hint`. Otherwise, the
  /// type of `e` is inferred as `defaultType`.
  ///
  /// - Requires: `e` is a literal expression.
  private mutating func _inferredType<T: Expr>(
    ofLiteral literal: T.ID, withHint hint: AnyType? = nil, defaultingTo defaultType: AnyType,
    updating obligations: inout ProofObligations
  ) -> AnyType {
    guard let h = hint else {
      return constrain(literal, to: defaultType, in: &obligations)
    }

    // Fast path if `e` is the default type.
    if areEquivalent(defaultType, h, in: program[literal].scope) {
      return constrain(literal, to: h, in: &obligations)
    }

    let cause = ConstraintOrigin(.literal, at: program[literal].site)
    let t = ^freshVariable()
    let p = program.ast.coreTrait(forTypesExpressibleBy: T.self)!

    let preferred: ConstraintSet = [
      EqualityConstraint(t, defaultType, origin: cause),
      SubtypingConstraint(defaultType, h, origin: cause),
    ]
    let alternative: ConstraintSet = [
      EqualityConstraint(t, h, origin: cause),
      ConformanceConstraint(h, conformsTo: p, origin: cause),
    ]

    obligations.insert(
      DisjunctionConstraint(
        between: [
          .init(constraints: preferred, penalties: 0),
          .init(constraints: alternative, penalties: 1),
        ],
        origin: cause))

    return constrain(literal, to: t, in: &obligations)
  }

  /// Returns the inferred type of `e`'s output, updating `obligations` and gathering contextual
  /// information from `hint`.
  private mutating func inferredReturnType(
    of e: LambdaExpr.ID, withHint hint: AnyType? = nil,
    updating obligations: inout ProofObligations
  ) -> AnyType {
    if let o = program[e].decl.output {
      return evalTypeAnnotation(o)
    } else if case .expr(let b) = program[e].decl.body {
      return inferredType(of: b, withHint: hint, updating: &obligations)
    } else {
      report(.error(cannotInferComplexReturnTypeAt: program[e].decl.introducerSite))
      return .error
    }
  }

  /// Returns the inferred type of `p`, updating `obligations` and gathering contextual information
  /// from `hint`.
  private mutating func inferredType(
    of p: AnyPatternID, withHint hint: AnyType? = nil,
    updating obligations: inout ProofObligations
  ) -> AnyType {
    switch p.kind {
    case BindingPattern.self:
      return inferredType(of: BindingPattern.ID(p)!, withHint: hint, updating: &obligations)
    case ExprPattern.self:
      return inferredType(of: ExprPattern.ID(p)!, withHint: hint, updating: &obligations)
    case NamePattern.self:
      return inferredType(of: NamePattern.ID(p)!, withHint: hint, updating: &obligations)
    case TuplePattern.self:
      return inferredType(of: TuplePattern.ID(p)!, withHint: hint, updating: &obligations)
    case WildcardPattern.self:
      return inferredType(of: WildcardPattern.ID(p)!, withHint: hint, updating: &obligations)
    default:
      unexpected(p, in: program.ast)
    }
  }

  /// Returns the inferred type of `p`, updating `obligations` and gathering contextual information
  /// from `hint`.
  private mutating func inferredType(
    of p: BindingPattern.ID, withHint hint: AnyType? = nil,
    updating obligations: inout ProofObligations
  ) -> AnyType {
    // A binding pattern introduces additional type information when it has a type annotation. In
    // that case, the type denoted by the annotation is used to infer the type of the sub-pattern
    // and constrained to be a subtype of the expected type, if any.
    var subpattern = hint
    if let a = program[p].annotation {
      let subject = evalTypeAnnotation(a)
      if let t = hint {
        obligations.insert(
          SubtypingConstraint(subject, t, origin: .init(.annotation, at: program[p].site)))
      }
      subpattern = subject
    }

    return inferredType(of: program[p].subpattern, withHint: subpattern, updating: &obligations)
  }

  /// Returns the inferred type of `p`, updating `obligations` and gathering contextual information
  /// from `hint`.
  private mutating func inferredType(
    of p: ExprPattern.ID, withHint hint: AnyType? = nil,
    updating obligations: inout ProofObligations
  ) -> AnyType {
    inferredType(of: program[p].expr, withHint: hint, updating: &obligations)
  }

  /// Returns the inferred type of `p`, updating `obligations` and gathering contextual information
  /// from `hint`.
  private mutating func inferredType(
    of p: NamePattern.ID, withHint hint: AnyType? = nil,
    updating obligations: inout ProofObligations
  ) -> AnyType {
    // Note: `declType` is set only if we're visiting the containing pattern more than once.
    let t = cache.local.declType[program[p].decl] ?? hint ?? ^freshVariable()
    obligations.assign(t, to: AnyDeclID(program[p].decl))
    return t
  }

  /// Returns the inferred type of `p`, updating `obligations` and gathering contextual information
  /// from `hint`.
  private mutating func inferredType(
    of p: TuplePattern.ID, withHint hint: AnyType? = nil,
    updating obligations: inout ProofObligations
  ) -> AnyType {
    // Infer the shape of the expected type if we don't a context.
    guard let h = hint else {
      return ^TupleType(
        program[p].elements.map { (a) in
          let t = inferredType(of: a.pattern, updating: &obligations)
          return .init(label: a.label?.value, type: t)
        })
    }

    // If we have a context, it must be a tuple or a variable.
    if let t = TupleType(h) {
      if t.elements.count != program[p].elements.count {
        return skip(.error(invalidDestructuringOfType: h, at: program[p].site))
      }

      var lhs: [String?] = []
      var rhs: [String?] = []

      // Visit the elements pairwise.
      for (a, b) in zip(program[p].elements, t.elements) {
        let t = inferredType(of: a.pattern, withHint: b.type, updating: &obligations)
        if t.isError { return .error }
        lhs.append(a.label?.value)
        rhs.append(b.label)
      }

      // Check that labels match.
      if lhs != rhs {
        report(.error(labels: lhs, incompatibleWith: rhs, at: program[p].site))
        return .error
      }
    } else if !h.isTypeVariable {
      return skip(.error(invalidDestructuringOfType: h, at: program[p].site))
    }

    return h

    /// Reports `d`, assigns all declarations in `p` to `.error`, and returns `.error`.
    func skip(_ d: Diagnostic) -> AnyType {
      for (_, n) in program.ast.names(in: p) {
        let t = cache.local.declType[program[n].decl] ?? .error
        obligations.assign(t, to: AnyDeclID(program[n].decl))
      }
      report(d)
      return .error
    }
  }

  /// Returns the inferred type of `p`, updating `obligations` and gathering contextual information
  /// from `hint`.
  private mutating func inferredType(
    of p: WildcardPattern.ID, withHint hint: AnyType? = nil,
    updating obligations: inout ProofObligations
  ) -> AnyType {
    hint ?? ^freshVariable()
  }

  /// Inserts in `obligations` the constraints implied by the result of name resolution for each
  /// nominal component in `components`.
  private mutating func constrain(
    _ components: [NameResolutionResult.ResolvedComponent], in obligations: inout ProofObligations
  ) -> AnyType {
    var last: AnyType?
    var substitutions: [GenericParameterDecl.ID: AnyType] = [:]
    for p in components {
      last = constrain(p.component, to: p.candidates, in: &obligations, updating: &substitutions)
    }
    return last!
  }

  /// Inserts in `obligations` the constraint that `name` refers to one of the declarations in
  /// `candidates`, updating `substitutions` with opened generic parameters, and returning the
  /// inferred type of `name`.
  ///
  /// - Requires: `candidates` is not empty
  private mutating func constrain(
    _ name: NameExpr.ID, to candidates: [NameResolutionResult.Candidate],
    in obligations: inout ProofObligations,
    updating substitutions: inout [GenericParameterDecl.ID: AnyType]
  ) -> AnyType {
    precondition(!candidates.isEmpty)
    let site = program[name].site

    // If there's only one candidate, commit to this choice.
    if var pick = candidates.uniqueElement {
      pick = instantiate(
        pick, in: program[name].scope,
        updating: &substitutions,
        anchoringInstantiationConstraintsAt: .init(.binding, at: site))
      obligations.assign(pick.reference, to: name)
      obligations.insert(pick.constraints)
      return constrain(name, to: pick.type, in: &obligations)
    }

    // Otherwise, create an overload set.
    let overloads: [OverloadConstraint.Predicate] = candidates.map({ (candidate) in
      let penalties = candidate.reference.decl.map({ program.isRequirement($0) ? 1 : 0 }) ?? 0
      let pick = instantiate(
        candidate, in: program[name].scope,
        updating: &substitutions,
        anchoringInstantiationConstraintsAt: .init(.binding, at: site))
      return .init(pick, penalties: penalties)
    })

    // Constrain the name to refer to one of the overloads.
    let t = ^freshVariable()
    let overload = OverloadConstraint(
      name, withType: t, refersToOneOf: overloads, origin: .init(.binding, at: site))
    obligations.insert(overload)
    return constrain(name, to: t, in: &obligations)
  }

  /// Constrains `subject` to have type `t` in `obligations`, returning `cache.local.exprType[e]`
  /// if it isn't `nil` or `t` otherwise.
  ///
  /// - Note: Unlike `assign(_:to:)`, this method doesn't override `inferredTypes[subject]` if
  ///   it isn't `nil` but creates an equality constraint instead.
  @discardableResult
  private mutating func constrain<T: ExprID>(
    _ e: T, to t: AnyType, in obligations: inout ProofObligations
  ) -> AnyType {
    if t[.hasError] { obligations.setUnsatisfiable() }
    assert(cache.local.exprType[e] == nil, "type already inferred")

    // Accumulate constraints on previous choices.
    if let u = obligations.exprType[e] {
      if areEquivalent(t, u, in: program[e].scope) { return u }
      obligations.insert(EqualityConstraint(t, u, origin: .init(.structural, at: program[e].site)))
      return u
    }

    obligations.assign(t, to: AnyExprID(e))
    return t
  }

  /// Proves the formulae in `obligations`, which relate to the well-typedness of `n`, returning
  /// the best assignment of universally quantified variables.
  @discardableResult
  private mutating func discharge<T: NodeIDProtocol>(
    _ obligations: ProofObligations, relatedTo n: T,
    ignoringSharedCache ignoreSharedCache: Bool = false
  ) -> Solution {
    // Compute the solution.
    let solution = tracingInference(relatedTo: n) { (me, isLoggingEnabled) in
      // Nothing to do if the obligations are known unsatisfiable.
      if obligations.isUnsatisfiable { return .init() }

      var system = ConstraintSystem(obligations, logging: isLoggingEnabled)
      return system.solution(querying: &me)
    }

    commit(solution, satisfying: obligations, ignoringSharedCache: ignoreSharedCache)
    return solution
  }

  /// Commits the choices made in `solution` to satisfy `obligations`.
  private mutating func commit(
    _ solution: Solution, satisfying obligations: ProofObligations,
    ignoringSharedCache ignoreSharedCache: Bool
  ) {
    for (n, r) in solution.bindingAssumptions {
      var s = solution.typeAssumptions.reify(r, withVariables: .kept)

      let t = solution.typeAssumptions.reify(obligations.exprType[n]!, withVariables: .kept)
      if t[.hasVariable] || s.arguments.values.contains(where: { $0.isTypeVariable }) {
        report(.error(notEnoughContextToInferArgumentsAt: program[n].site))
        s = solution.typeAssumptions.reify(s, withVariables: .substitutedByError)
      }

      cache.write(s, at: \.referredDecl[n], ignoringSharedCache: ignoreSharedCache)
    }

    for (e, t) in obligations.exprType {
      let u = solution.typeAssumptions.reify(t, withVariables: .substitutedByError)
      cache.write(u, at: \.exprType[e], ignoringSharedCache: ignoreSharedCache)
    }

    // TODO: Patterns

    // Note: Post-inference checks run after other choices have been committed.
    for (d, t) in obligations.declType {
      let u = solution.typeAssumptions.reify(t, withVariables: .substitutedByError)
      cache.uncheckedType[d].updateMonotonically(.computed(u))
      checkPostInference(d, solution: solution, ignoringSharedCache: ignoreSharedCache)
    }

    report(solution.diagnostics.elements)
    assert(solution.isSound || diagnostics.containsError, "inference failed without diagnostics")
  }

  /// Calls `action` on `self`, logging a trace of constraint solving iff `shouldTraceInference(n)`
  /// returns `true`.
  private mutating func tracingInference<T: NodeIDProtocol>(
    relatedTo n: T, _ action: (inout Self, Bool) -> Solution
  ) -> Solution {
    guard let f = shouldTraceInference, f(AnyNodeID(n), program) else {
      return action(&self, false)
    }

    print("\(program[n].site): type inference for '\(program[n])'")
    print("---")
    let s = action(&self, true)
    print(s)
    print("---")
    return s
  }

  /// Returns the rank of `lhs` relative ro `rhs`.
  mutating func rank(_ lhs: Solution, _ rhs: Solution) -> StrictPartialOrdering {
    var ranking: StrictPartialOrdering = .equal
    var namesInCommon = 0

    for (n, lhsDeclRef) in lhs.bindingAssumptions {
      guard let rhsDeclRef = rhs.bindingAssumptions[n] else { continue }
      namesInCommon += 1

      // Nothing to do if both functions have the binding.
      if lhsDeclRef == rhsDeclRef { continue }
      let lhs = uncheckedType(of: lhsDeclRef.decl!)
      let rhs = uncheckedType(of: rhsDeclRef.decl!)

      switch compareSpecificity(lhs, rhs, in: program[n].scope, at: program[n].site) {
      case .ascending:
        if ranking == .descending { return nil }
        ranking = .ascending
      case .descending:
        if ranking == .ascending { return nil }
        ranking = .descending
      default:
        return nil
      }
    }

    if lhs.bindingAssumptions.count < rhs.bindingAssumptions.count {
      if namesInCommon == lhs.bindingAssumptions.count {
        return ranking != .ascending ? .descending : nil
      } else {
        return nil
      }
    }

    if lhs.bindingAssumptions.count > rhs.bindingAssumptions.count {
      if namesInCommon == rhs.bindingAssumptions.count {
        return ranking != .descending ? .ascending : nil
      } else {
        return nil
      }
    }

    return namesInCommon == lhs.bindingAssumptions.count ? ranking : nil
  }

  /// Compares `lhs` and `rhs` and returns whether one is more specific than the other in
  /// `scopeOfUse`, instantiating generic type constraints at `site`.
  ///
  /// `t1` is more specific than `t2` if both are callable types with the same labels and `t1`
  /// accepts stricly less arguments than `t2`.
  private mutating func compareSpecificity(
    _ lhs: AnyType, _ rhs: AnyType, in scopeOfUse: AnyScopeID, at site: SourceRange
  ) -> StrictPartialOrdering {
    guard
      let l = lhs.base as? CallableType,
      let r = rhs.base as? CallableType
    else { return nil }

    guard
      l.inputs.count == r.inputs.count,
      l.inputs.elementsEqual(r.inputs, by: { $0.label == $1.label })
    else { return nil }

    let lRefinesR = isMoreSpecific(lhs, rhs, in: scopeOfUse, at: site)
    let rRefinesL = isMoreSpecific(rhs, lhs, in: scopeOfUse, at: site)

    if lRefinesR {
      return rRefinesL ? nil : .ascending
    }
    if rRefinesL {
      return lRefinesR ? nil : .descending
    }
    return nil
  }

  /// Returns `true` iff `lhs` is more specific than `rhs` in `scopeOfUse`, instantiating generic
  /// type constraints at `site`.
  ///
  /// - Requires: `lhs` and `rhs` are `CallableType`s.
  private mutating func isMoreSpecific(
    _ lhs: AnyType, _ rhs: AnyType, in scopeOfUse: AnyScopeID, at site: SourceRange
  ) -> Bool {
    // Open the right operand.
    let openedRight = open(type: rhs, at: site)
    var constraints = openedRight.constraints

    // Create pairwise subtyping constraints on the parameters.
    let l = lhs.base as! CallableType
    let r = openedRight.shape.base as! CallableType
    for (a, b) in zip(l.inputs, r.inputs) {
      constraints.insert(SubtypingConstraint(a.type, b.type, origin: .init(.binding, at: site)))
    }

    // Solve the constraint system.
    var obligations = ProofObligations(scope: scopeOfUse)
    obligations.insert(constraints)

    var s = ConstraintSystem(obligations, logging: false)
    return !s.solution(querying: &self).isSound
  }

  // MARK: AST Restructuring

  /// Returns a binary tree encoding the evaluation order of `e` or `nil` if `e` contains an
  /// undefined operator.
  private mutating func fold(_ e: SequenceExpr.ID) -> FoldedSequenceExpr? {
    if let tree = cache.local.foldedForm[e] { return tree }

    guard let tree = fold(program[e].tail[0...], into: .leaf(program[e].head)) else { return nil }
    cache.write(tree, at: \.foldedForm[e], ignoringSharedCache: true)
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
      let stem = program[tail[i].operator].name.value.stem
      let candidates = lookup(operator: stem, used: .infix, in: program[tail[i].operator].scope)

      switch candidates.count {
      case 0:
        report(.error(undefinedOperator: stem, at: program[tail[i].operator].site))
        return nil

      case 1:
        let precedence = program[candidates[0]].precedenceGroup?.value
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

  // MARK: Helpers

  /// Creates a fresh type variable.
  ///
  /// The returned instance is unique accress concurrent type checker instances.
  mutating func freshVariable() -> TypeVariable {
    defer { nextFreshVariableIdentifier += 1 }
    return .init(nextFreshVariableIdentifier)
  }

  /// Returns `true` iff `t` is known as an arrow type.
  private func isArrow(_ t: AnyType) -> Bool {
    (t.base as? CallableType)?.isArrow ?? false
  }

  /// Returns `true` iff `t` is known as a subscript type.
  private func isSubscript(_ t: AnyType) -> Bool {
    !isArrow(t)
  }

  /// Returns `true` iff `e` is bound to a nominal type declaration in `obligations`.
  private mutating func isBoundToNominalTypeDecl(
    _ e: AnyExprID, in obligations: ProofObligations
  ) -> Bool {
    guard
      let c = NameExpr.ID(e),
      let r = obligations.referredDecl[c]
    else { return false }
    return isBoundToNominalTypeDecl(r)
  }

  /// Returns `true` iff `r` is bound to a nominal type declaration.
  private mutating func isBoundToNominalTypeDecl(_ r: DeclReference) -> Bool {
    if let d = r.decl {
      return isNominalTypeDecl(d)
    } else {
      return (r == .builtinType) || (r == .compilerKnownType)
    }
  }

  /// Returns `true` iff `d` is a nominal type declaration.
  private mutating func isNominalTypeDecl(_ d: AnyDeclID) -> Bool {
    switch d.kind {
    case AssociatedTypeDecl.self, ProductTypeDecl.self, TypeAliasDecl.self:
      return true
    case GenericParameterDecl.self:
      return uncheckedType(of: d).base is MetatypeType
    default:
      return false
    }
  }

  /// Returns `true` iff `d` introduces `var` bindings.
  private func isVar(_ d: BindingDecl.ID) -> Bool {
    program[d].pattern.introducer.value == .var
  }

  /// If `t` is the type of a mutating bundle in `scopeOfUse`, returns the output of a mutating
  /// variant in that bundle. Otherwise, returns `nil`.
  private mutating func mutatingVariantOutput(
    of t: MethodType, in scopeOfUse: AnyScopeID
  ) -> AnyType? {
    guard
      let es = TupleType(canonical(t.output, in: scopeOfUse))?.elements,
      (es.count == 2) && (es[0].label == "self") && (es[1].label == nil),
      areEquivalent(es[0].type, t.receiver, in: scopeOfUse)
    else { return nil }
    return es[1].type
  }

  // MARK: Caching

  /// A possibly shared instance of a typed program.
  struct Cache {

    /// A lookup table.
    typealias LookupTable = [String: Set<AnyDeclID>]

    /// A key in a member lookup table.
    typealias MemberLookupKey = ScopedValue<AnyType>

    /// The local instance being type checked.
    private(set) var local: TypedProgram

    /// The instance being collaboratively typed checked.
    private var shared: SharedMutable<TypedProgram>?

    /// The updates of the shared cache currently pending.
    private var pendingUpdates: [SynchronizationUpdate] = []

    /// The number of updates in `pendingUpdates` that should be synchronized early.
    private var earlyUpdateCount = 0

    /// The number of early updates that are accumulated before being synchronized.
    private static let earlyUpdateThreshold = 100

    /// The declarations being currently type checked.
    var declsUnderChecking = Set<AnyDeclID>()

    /// A map from declaration to its overarching type before it's been type checked.
    var uncheckedType = DeclProperty<Memo<AnyType>>()

    /// A map from declaration space to its members.
    ///
    /// This map serves as cache for `lookup(_:memberOf:exposedTo)`. At no point is it guaranteed
    /// to be complete.
    var scopeToMembers: [MemberLookupKey: LookupTable] = [:]

    /// A map from lexical scope to the names introduced in it.
    ///
    /// This map serves as cache for `names(introducedIn:)`.
    var scopeToNames: [AnyScopeID: LookupTable] = [:]

    /// Creates an instance for memoizing type checking results in `local` and comminicating them
    /// to concurrent type checkers using `shared`.
    init(local: TypedProgram, shared: SharedMutable<TypedProgram>? = nil) {
      self.local = local
      self.shared = shared
    }

    /// Returns the value of the cache at `path`.
    ///
    /// The value at `path` in `self.local` is read and returned if it isn't `nil`. Otherwise, the
    /// value at `path` in `self.shared` is read, stored in `self.local`, and returned if it isn't
    /// `nil`, unless `self.cache` is `nil` or `ignoreSharedCache` is true.
    mutating func read<V>(
      _ path: WritableKeyPath<TypedProgram, V?>,
      ignoringSharedCache ignoreSharedCache: Bool = false
    ) -> V? {
      if let v = local[keyPath: path] {
        return v
      } else if !ignoreSharedCache, let v = shared?.wrapped[keyPath: path] {
        local[keyPath: path] = v
        return v
      } else {
        return nil
      }
    }

    /// Writes `value` to the cache at `path`, applying the update with `merge`.
    ///
    /// After the call, `self.local[keyPath: path] == value`. If `self.shared` isn't `nil`, the
    /// update is recorded for the next synchronization round and `self.earlyUpdateCount` is
    /// incremented, unless `ignoreSharedCache` is `true`.
    ///
    /// `merge` asserts that the update is monotonic.
    mutating func write<V>(
      _ value: V, at path: WritableKeyPath<TypedProgram, V>,
      ignoringSharedCache ignoreSharedCache: Bool = false,
      mergingWith merge: @escaping (inout V, V) -> Void
    ) {
      local.write(value, at: path, mergingWith: merge)

      if shared != nil {
        func update(
          _ value: Any, at path: PartialKeyPath<TypedProgram>, in program: inout TypedProgram
        ) {
          let p = path as! WritableKeyPath<TypedProgram, V>
          merge(&program[keyPath: p], value as! V)
        }

        pendingUpdates.append(.init(path, applying: update))
        if !ignoreSharedCache {
          earlyUpdateCount += 1
        }
        synchronizeIfThresholdReached()
      }
    }

    /// Writes `value` to the cache at `path`, applying the update with `merge`.
    ///
    /// After the call, `self.local[keyPath: path] == value`. If `self.shared` isn't `nil`, the
    /// update is recorded for the next synchronization round and `self.earlyUpdateCount` is
    /// incremented, unless `ignoreSharedCache` is `true`.
    mutating func write<V>(
      _ value: V, at path: WritableKeyPath<TypedProgram, V>,
      ignoringSharedCache ignoreSharedCache: Bool = false
    ) where V: Monotonic {
      write(value, at: path) { (u, v) in u.updateMonotonically(v) }
    }

    /// Applies `self.read(path)`.
    subscript<V>(m: WritableKeyPath<TypedProgram, V?>) -> V? where V: Equatable {
      mutating get { read(m) }
    }

    /// Applies all pending updates to the shared instance iff `earlyUpdateThreshold` has been
    /// reached, resetting `self.earlyUpdateCount`.
    private mutating func synchronizeIfThresholdReached() {
      if earlyUpdateCount >= Self.earlyUpdateThreshold { synchronize() }
    }

    /// Applies all pending updates to the shared instance.
    fileprivate mutating func synchronize() {
      shared?.modify { (p) in
        for u in pendingUpdates {
          u.apply(local[keyPath: u.pathToUpdate], u.pathToUpdate, &p)
        }
      }
      pendingUpdates.removeAll(keepingCapacity: true)
      earlyUpdateCount = 0
    }

  }

  /// A synchronization update to perform on a shared instance.
  private struct SynchronizationUpdate: Hashable {

    /// A closure that accepts a `path` in `instance` and a `value` to write.
    typealias Apply = (
      _ value: Any,
      _ path: PartialKeyPath<TypedProgram>,
      _ instance: inout TypedProgram
    ) -> Void

    /// The path to the value to update.
    let pathToUpdate: PartialKeyPath<TypedProgram>

    /// A closure executing the update.
    let apply: Apply

    /// Hashes the salient parts of `self` into `hasher`.
    func hash(into hasher: inout Hasher) {
      hasher.combine(pathToUpdate)
    }

    /// Returns `true` iff `l` is equal to `r`.
    static func == (l: Self, r: Self) -> Bool {
      l.pathToUpdate == r.pathToUpdate
    }

    /// Creates an instance that writes `value` at `pathToUpdate` in a typed program.
    init(_ pathToUpdate: PartialKeyPath<TypedProgram>, applying apply: @escaping Apply) {
      self.pathToUpdate = pathToUpdate
      self.apply = apply
    }

  }

}

extension Program {

  /// Returns a reference to `d` as a member of `parent`, specialized by `specialization`, exposed
  /// to `scopeOfUse`, and used as a constructor if `isConstructor` is `true`.
  fileprivate func makeReference(
    to d: AnyDeclID, specializedBy specialization: GenericArguments,
    memberOf parent: NameResolutionContext?,
    exposedTo scopeOfUse: AnyScopeID,
    usedAsConstructor isConstructor: Bool
  ) -> DeclReference {
    if isConstructor {
      return .constructor(InitializerDecl.ID(d)!, specialization)
    } else if isNonStaticMember(d) && !(parent?.type.base is MetatypeType) {
      if let p = parent {
        return .member(d, specialization, p.receiver!)
      } else {
        let r = innermostReceiver(in: scopeOfUse)!
        return .member(d, specialization, .elided(.direct(AnyDeclID(r), [:])))
      }
    } else {
      return .direct(d, specialization)
    }
  }

}
