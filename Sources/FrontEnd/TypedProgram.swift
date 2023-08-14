import Core
import Foundation
import Utils

/// A data structure representing a typed Hylo program ready to be lowered.
public struct TypedProgram {

  /// A set of conformances represented to answer "does A conform to T in S" efficiently.
  public typealias ConformanceSet = [AnyType: ConformanceTable]

  /// A table from trait to its conformances for a given type.
  public typealias ConformanceTable = [TraitType: Set<Conformance>]

  /// The program annotated by the properties of `self`.
  private let base: ScopedProgram

  /// A map from translation unit to its imports.
  public internal(set) var imports: [TranslationUnit.ID: Set<ModuleDecl.ID>] = [:]

  /// The overarching type of each declaration.
  public internal(set) var declType: [AnyDeclID: AnyType] = [:]

  /// The type of each expression.
  public internal(set) var exprType: [AnyExprID: AnyType] = [:]

  /// The type of each pattern.
  public internal(set) var patternType: [AnyPatternID: AnyType] = [:]

  /// A map from function and subscript declarations to their implicit captures.
  public internal(set) var implicitCaptures: [AnyDeclID: Set<ImplicitCapture>] = [:]

  /// A map from generic declarations to their environment.
  public internal(set) var environment: [AnyDeclID: GenericEnvironment] = [:]

  /// A map from module to its synthesized declarations.
  public internal(set) var synthesizedDecls: [ModuleDecl.ID: Set<SynthesizedFunctionDecl>] = [:]

  /// A map from name expression to its referred declaration.
  public internal(set) var referredDecl: BindingMap = [:]

  /// A map from sequence expressions to their evaluation order.
  public internal(set) var foldedForm: [SequenceExpr.ID: FoldedSequenceExpr] = [:]

  /// The conformances in the program.
  public internal(set) var conformances: ConformanceSet = [:]

  /// Creates an instance annotating `base` with semantic information, reporting diagnostics to
  /// `log` and throwing iff an error was found.
  ///
  /// - Parameters:
  ///   - isTypeCheckingParallel: if `true`, the program is partitioned into chucks that are type
  ///     checked separately. Otherwise, type checking is performed sequentially. Either way, the
  ///     order in which declarations are being checked is undeterministic.
  ///   - shouldTraceInference: A closure accepting a node and its containing program, returning
  ///     `true` if a trace of type inference should be logged on the console for that node. The
  ///     closure is not called if `isTypeCheckingParallel` is `true`.
  public init(
    annotating base: ScopedProgram,
    inParallel isTypeCheckingParallel: Bool = false,
    reportingDiagnosticsTo log: inout DiagnosticSet,
    tracingInferenceIf shouldTraceInference: ((AnyNodeID, TypedProgram) -> Bool)? = nil
  ) throws {
    let instanceUnderConstruction = SharedMutable(TypedProgram(partiallyFormedFrom: base))

    if isTypeCheckingParallel {
      let sources = base.ast[base.ast.modules].map(\.sources).joined()
      var tasks: [TypeCheckTask] = []

      for (i, chunk) in sources.chunked(inMax: 255).enumerated() {
        let t = TypeCheckTask(
          Array(chunk), withCheckerIdentifiedBy: UInt8(i),
          collaborativelyConstructing: instanceUnderConstruction,
          tracingInferenceIf: nil)
        tasks.append(t)
      }

      let queue = OperationQueue()
      queue.addOperations(tasks, waitUntilFinished: true)
      for t in tasks {
        log.formUnion(t.diagnostics)
      }
    }

    var checker = TypeChecker(
      constructing: instanceUnderConstruction.wrapped,
      tracingInferenceIf: isTypeCheckingParallel ? nil : shouldTraceInference)
    checker.checkAllDeclarations()

    log.formUnion(checker.diagnostics)
    try log.throwOnError()
    self = checker.program
  }

  /// The type checking of a collection of source files.
  private final class TypeCheckTask: Operation {

    /// The sources to check.
    private let sources: [TranslationUnit.ID]

    /// The checker used by this operation.
    private var checker: TypeChecker

    /// Creates a task type checking `sources` with a checker identified by `checkerIdentifier`,
    /// constructing `instanceUnderConstruction`.
    init(
      _ sources: [TranslationUnit.ID],
      withCheckerIdentifiedBy checkerIdentifier: UInt8,
      collaborativelyConstructing instanceUnderConstruction: SharedMutable<TypedProgram>,
      tracingInferenceIf shouldTraceInference: ((AnyNodeID, TypedProgram) -> Bool)?
    ) {
      self.sources = sources
      self.checker = TypeChecker(
        checkerIdentifier, collaborativelyConstructing: instanceUnderConstruction,
        tracingInferenceIf: shouldTraceInference)
    }

    /// Executes the operation.
    override func main() {
      checker.check(sources)
      checker.synchronize()
    }

    /// The diagnostic reported during type checker.
    var diagnostics: DiagnosticSet {
      checker.diagnostics
    }

  }

  /// Creates a partially formed instance with empty property in preparation for type checking.
  private init(partiallyFormedFrom base: ScopedProgram) {
    self.base = base
  }

  /// Returns the canonical form of `t` in `scopeOfUse`.
  public func canonical(_ t: AnyType, in scopeOfUse: AnyScopeID) -> AnyType {
    if t[.isCanonical] { return t }
    var checker = TypeChecker(asContextFor: self)
    return checker.canonical(t, in: scopeOfUse)
  }

  /// Returns `arguments` with all types replaced by their canonical form in `scopeOfUse`.
  public func canonical(
    _ arguments: GenericArguments, in scopeOfUse: AnyScopeID
  ) -> GenericArguments {
    var checker = TypeChecker(asContextFor: self)
    return checker.canonical(arguments, in: scopeOfUse)
  }

  /// Returns `true` iff `t` and `u` are equivalent types in `scopeOfUse`.
  public func areEquivalent(_ t: AnyType, _ u: AnyType, in scopeOfUse: AnyScopeID) -> Bool {
    var checker = TypeChecker(asContextFor: self)
    return checker.canonical(t, in: scopeOfUse) == checker.canonical(u, in: scopeOfUse)
  }

  /// Returns `generic` with occurrences of parameters keying `specialization` replaced by their
  /// corresponding value, performing necessary name lookups from `scopeOfUse`.
  ///
  /// This method has no effect if `substitutions` is empty.
  public func specialize(
    _ generic: AnyType, for specialization: GenericArguments, in scopeOfUse: AnyScopeID
  ) -> AnyType {
    var checker = TypeChecker(asContextFor: self)
    return checker.specialize(generic, for: specialization, in: scopeOfUse)
  }

  /// Returns `arguments` applying `specialization` on each value in `scopeOfUse`.
  public func specialize(
    _ arguments: GenericArguments, for specialization: GenericArguments,
    in scopeOfUse: AnyScopeID
  ) -> GenericArguments {
    var checker = TypeChecker(asContextFor: self)
    return arguments.mapValues { (v) in
      if let t = v as? AnyType {
        return checker.specialize(t, for: specialization, in: scopeOfUse)
      } else {
        fatalError("not implemented")
      }
    }
  }

  /// Returns `true` iff deinitializing an instance of `t` in `scopeOfUse` is a no-op.
  ///
  /// A type is trivially deinitializable in a scope `s` if it is built-in, or if its conformance
  /// to `Deinitializable` exposed to `s` does not involve any user-defined function.
  public func isTriviallyDeinitializable(_ t: AnyType, in scopeOfUse: AnyScopeID) -> Bool {
    let model = canonical(t, in: scopeOfUse)

    guard
      let c = conformance(of: model, to: ast.deinitializableTrait, exposedTo: scopeOfUse),
      case .synthetic = c.implementations.uniqueElement!.value
    else {
      switch model.base {
      case is BuiltinType:
        // Built-in types never have conformances.
        return true

      // FIXME: (see #855)
      case let u as LambdaType:
        return u.environment == .void
      case is MetatypeType:
        return true

      default:
        return false
      }
    }

    switch model.base {
    case is BuiltinType:
      return true
    case let u as TupleType:
      return u.elements.allSatisfy({ isTriviallyDeinitializable($0.type, in: scopeOfUse) })
    case let u as UnionType:
      return u.elements.allSatisfy({ isTriviallyDeinitializable($0, in: scopeOfUse) })
    default:
      return false
    }
  }

  /// If `t` has a record layout, returns the names and types of its stored properties.
  public func storage(of t: AnyType) -> [TupleType.Element] {
    switch t.base {
    case let u as BoundGenericType:
      return storage(of: u.base)
    case let u as ProductType:
      return storage(of: u)
    case let u as TupleType:
      return u.elements
    default:
      assert(!t.hasRecordLayout)
      return []
    }
  }

  /// If `t` has a record layout, returns the names and types of its stored properties.
  public func storage(of t: ProductType) -> [TupleType.Element] {
    var result: [TupleType.Element] = []
    for b in ast[t.decl].members.filter(BindingDecl.self) {
      for (_, n) in ast.names(in: ast[b].pattern) {
        result.append(.init(label: ast[ast[n].decl].baseName, type: declType[ast[n].decl]!))
      }
    }
    return result
  }

  /// Returns generic parameters captured by `s` and the scopes semantically containing `s`.
  ///
  /// A declaration may take generic parameters even if it doesn't declare any. For example, a
  /// nested function will implicitly capture the generic parameters introduced in its context.
  ///
  /// Parameters are returned outer to inner, left to right: the first parameter of the outermost
  /// generic scope appears first; the last parameter of the innermost generic scope appears last.
  public func accumulatedGenericParameters<T: ScopeID>(
    in s: T
  ) -> ReversedCollection<[GenericParameterDecl.ID]> {
    var result: [GenericParameterDecl.ID] = []
    appendGenericParameters(in: s, to: &result)
    return result.reversed()
  }

  /// Appends generic parameters captured by `s` and the scopes semantically containing `s` to
  /// `accumulatedParameters`, right to left, inner to outer.
  private func appendGenericParameters<T: ScopeID>(
    in s: T, to accumulatedParameters: inout [GenericParameterDecl.ID]
  ) {
    switch s.kind.value {
    case is ConformanceDecl.Type:
      appendGenericParameters(in: ConformanceDecl.ID(s)!, to: &accumulatedParameters)
    case is ExtensionDecl.Type:
      appendGenericParameters(in: ExtensionDecl.ID(s)!, to: &accumulatedParameters)
    case is GenericScope.Type:
      accumulatedParameters.append(contentsOf: environment[AnyDeclID(s)!]!.parameters)
    case is TranslationUnit.Type, is ModuleDecl.Type:
      return
    default:
      break
    }

    appendGenericParameters(in: nodeToScope[s]!, to: &accumulatedParameters)
  }

  /// Appends generic parameters captured by `s` and the scopes semantically containing `s` to
  /// `accumulatedParameters`, right to left, inner to outer.
  private func appendGenericParameters<T: TypeExtendingDecl>(
    in d: T.ID, to accumulatedParameters: inout [GenericParameterDecl.ID]
  ) {
    guard let p = scopeExtended(by: d) else { return }
    appendGenericParameters(in: p, to: &accumulatedParameters)
  }

  /// Returns `true` iff `model` conforms to `concept` in `scopeOfUse`.
  public func conforms(
    _ model: AnyType, to concept: TraitType, in scopeOfUse: AnyScopeID
  ) -> Bool {
    conformance(of: model, to: concept, exposedTo: scopeOfUse) != nil
  }

  /// Returns the conformance of `model` to `concept` that is exposed to `scopeOfUse`, or `nil` if
  /// such a conformance doesn't exist.
  public func conformance(
    of model: AnyType, to concept: TraitType, exposedTo scopeOfUse: AnyScopeID
  ) -> Conformance? {
    let m = canonical(model, in: scopeOfUse)
    if let c = declaredConformance(of: m, to: concept, exposedTo: scopeOfUse) {
      return c
    } else {
      return structuralConformance(of: m, to: concept, exposedTo: scopeOfUse)
    }
  }

  /// Returns the explicitly declared conformance of `model` to `concept` that is exposed to
  /// `scopeOfUse`, or `nil` if such a conformance doesn't exist.
  ///
  /// This method returns `nil` if the conformance of `model` to `concept` is structural (e.g., a
  /// tuple's synthesized conformance to `Movable`).
  private func declaredConformance(
    of model: AnyType, to concept: TraitType, exposedTo scopeOfUse: AnyScopeID
  ) -> Conformance? {
    assert(model[.isCanonical])

    // `A<X>: T` iff `A: T`.
    if let t = BoundGenericType(model) {
      guard let c = conformance(of: t.base, to: concept, exposedTo: scopeOfUse) else {
        return nil
      }

      // TODO: translate generic arguments to conditions

      return .init(
        model: t.base, concept: concept, arguments: t.arguments, conditions: [],
        scope: c.scope, implementations: c.implementations, isStructural: c.isStructural,
        site: c.site)
    }

    guard
      let allConformances = conformances[model],
      let conformancesToConcept = allConformances[concept]
    else { return nil }

    // Return the first conformance exposed to `scopeOfUse`.
    let exposed = modules(exposedTo: scopeOfUse)
    return conformancesToConcept.first { (c) in
      if let m = ModuleDecl.ID(c.scope), exposed.contains(m) {
        return true
      } else {
        return isContained(scopeOfUse, in: c.scope)
      }
    }
  }

  /// Returns the implicit structural conformance of `model` to `concept` that is exposed to
  /// `scopeOfUse`, or `nil` if such a conformance doesn't exist.
  private func structuralConformance(
    of model: AnyType, to concept: TraitType, exposedTo scopeOfUse: AnyScopeID
  ) -> Conformance? {
    assert(model[.isCanonical])

    switch model.base {
    case is RemoteType:
      break

    case let m as TupleType:
      guard m.elements.allSatisfy({ conforms($0.type, to: concept, in: scopeOfUse) }) else {
        return nil
      }

    case let m as UnionType:
      guard m.elements.allSatisfy({ conforms($0, to: concept, in: scopeOfUse) }) else {
        return nil
      }

    default:
      return nil
    }

    var implementations = Conformance.ImplementationMap()
    for requirement in ast.requirements(of: concept.decl) {
      guard let k = ast.synthesizedImplementation(of: requirement, definedBy: concept) else {
        return nil
      }
      let a: GenericArguments = [ast[concept.decl].selfParameterDecl: model]
      let t = LambdaType(specialize(declType[requirement]!, for: a, in: scopeOfUse))!
      let d = SynthesizedFunctionDecl(k, typed: t, in: scopeOfUse)
      implementations[requirement] = .synthetic(d)
    }

    return .init(
      model: model, concept: concept, arguments: [:], conditions: [], scope: scopeOfUse,
      implementations: implementations, isStructural: true,
      site: .empty(at: ast[scopeOfUse].site.first()))
  }

  /// Returns the scope of the declaration extended by `d`, if any.
  public func scopeExtended<T: TypeExtendingDecl>(by d: T.ID) -> AnyScopeID? {
    var checker = TypeChecker(asContextFor: self)
    return checker.scopeExtended(by: d)
  }

  /// Returns the modules visibles to `s`:
  private func modules(exposedTo s: AnyScopeID) -> Set<ModuleDecl.ID> {
    if let m = ModuleDecl.ID(s) {
      return [m]
    } else {
      return imports[source(containing: s), default: []]
    }
  }

  /// Returns the declarations of `d`' captures.
  ///
  /// If `d` is a member, its receiver is its only capture. Otherwise, this method returns
  /// `nonMemberCaptures(d)`.
  public func captures(of d: FunctionDecl.ID) -> [AnyDeclID] {
    if let r = ast[d].receiver {
      return [AnyDeclID(r)]
    }
    return nonMemberCaptures(of: d)
  }

  /// Returns the declarations of `d`' captures.
  ///
  /// If `d` is a member, its receiver is its only capture. Otherwise, this method returns
  /// `nonMemberCaptures(d)`.
  public func captures(of d: SubscriptImpl.ID) -> [AnyDeclID] {
    if let r = ast[d].receiver {
      return [AnyDeclID(r)]
    }
    return nonMemberCaptures(of: SubscriptDecl.ID(nodeToScope[d]!)!)
  }

  /// Returns the declarations of `d`'s captures.
  ///
  /// Explicit captures come first, in the order they appear in its capture list, from left to
  /// right. Implicit captures come next, in the order they were found during type checking.
  ///
  /// - Requires: `d` is not a member declaration.
  public func nonMemberCaptures<T: CapturingDecl>(of d: T.ID) -> [AnyDeclID] {
    var result: [AnyDeclID] = []
    for e in ast[d].explicitCaptures {
      for (_, n) in ast.names(in: ast[e].pattern) {
        result.append(AnyDeclID(ast[n].decl))
      }
    }
    for n in implicitCaptures[d]! {
      result.append(AnyDeclID(n.decl))
    }
    return result
  }

  /// Applies `merge(self[keyPath: path], value)`.
  ///
  /// - Parameter merge: A closure that merges `value` into the value currently stored at `path`,
  ///   guaranteeing that the result of the merge is a refinement.
  mutating func write<V>(
    _ value: V, at path: WritableKeyPath<Self, V>,
    mergingWith merge: (inout V, V) -> Void
  ) {
    modify(&self[keyPath: path], { (u) in merge(&u, value) })
  }

  /// Writes `value` at `path`.
  ///
  /// - Requires: `value` contains at least as much information as `self[keyPath: path]`.
  mutating func write<V>(
    _ value: V, at path: WritableKeyPath<Self, V>
  ) where V: Monotonic {
    write(value, at: path) { (u, v) in u.updateMonotonically(v) }
  }

}

extension TypedProgram: Program {

  /// The AST of the program.
  public var ast: AST { base.ast }

  /// A map from node to the innermost scope that contains it.
  public var nodeToScope: ASTProperty<AnyScopeID> { base.nodeToScope }

  /// A map from scope to the declarations directly contained in it.
  public var scopeToDecls: ASTProperty<[AnyDeclID]> { base.scopeToDecls }

  /// A map from variable declaration its containing binding declaration.
  public var varToBinding: [VarDecl.ID: BindingDecl.ID] { base.varToBinding }

}
