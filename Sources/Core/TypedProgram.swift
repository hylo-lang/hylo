/// A data structure representing a typed Val program ready to be lowered.
public struct TypedProgram: Program {

  public let ast: AST

  public let nodeToScope: ASTProperty<AnyScopeID>

  public let scopeToDecls: ASTProperty<[AnyDeclID]>

  public let varToBinding: [VarDecl.ID: BindingDecl.ID]

  /// A map from translation unit to its imports.
  public let imports: [TranslationUnit.ID: Set<ModuleDecl.ID>]

  /// The overarching type of each declaration.
  public let declType: DeclProperty<AnyType>

  /// The type of each expression.
  public let exprType: ExprProperty<AnyType>

  /// A map from function and subscript declarations to their implicit captures.
  public let implicitCaptures: DeclProperty<[ImplicitCapture]>

  /// A map from generic declarations to their environment.
  public let environment: DeclProperty<GenericEnvironment>

  /// A map from module to its synthesized declarations.
  ///
  /// This table contains the synthesized implementations of conformances declared explicitly.
  /// These declarations are unconditionally lowered to Val IR as they may be part of a module's
  /// API. Synthethized declarations that are part of a structural conformance are lowered lazily
  /// after mandatory IR passes and are not part of this table.
  public let synthesizedDecls: [ModuleDecl.ID: [SynthesizedFunctionDecl]]

  /// A map from name expression to its referred declaration.
  public let referredDecl: [NameExpr.ID: DeclReference]

  /// A map from sequence expressions to their evaluation order.
  public let foldedForm: [SequenceExpr.ID: FoldedSequenceExpr]

  /// The type relations of the program.
  public let relations: TypeRelations

  /// Creates a typed program from a scoped program and property maps describing type annotations.
  ///
  /// - Requires: All modules in `program` have been sucessfully typed checked.
  public init(
    annotating program: ScopedProgram,
    imports: [TranslationUnit.ID: Set<ModuleDecl.ID>],
    declTypes: DeclProperty<AnyType>,
    exprTypes: ExprProperty<AnyType>,
    implicitCaptures: DeclProperty<[ImplicitCapture]>,
    environments: DeclProperty<GenericEnvironment>,
    synthesizedDecls: [ModuleDecl.ID: [SynthesizedFunctionDecl]],
    referredDecls: [NameExpr.ID: DeclReference],
    foldedSequenceExprs: [SequenceExpr.ID: FoldedSequenceExpr],
    relations: TypeRelations
  ) {
    precondition(program.ast.modules.allSatisfy({ declTypes[$0]?.base is ModuleType }))

    self.ast = program.ast
    self.nodeToScope = program.nodeToScope
    self.scopeToDecls = program.scopeToDecls
    self.varToBinding = program.varToBinding
    self.imports = imports
    self.declType = declTypes
    self.exprType = exprTypes
    self.implicitCaptures = implicitCaptures
    self.environment = environments
    self.synthesizedDecls = synthesizedDecls
    self.referredDecl = referredDecls
    self.foldedForm = foldedSequenceExprs
    self.relations = relations
  }

  /// Returns the canonical type of `d`, parameterized by `a`.
  public func canonicalType<T: DeclID>(of d: T, parameterizedBy a: GenericArguments) -> AnyType {
    relations.canonical(relations.monomorphize(declType[d]!, for: a))
  }

  /// Returns the declarations of `d`' captures.
  ///
  /// If `d` is a member function, its receiver is its only capture. Otherwise, its explicit
  /// captures come first, in the order they appear in its capture list, from left to right.
  /// Implicit captures come next, in the order they were found during type checking.
  public func captures(of d: FunctionDecl.ID) -> [AnyDeclID] {
    var result: [AnyDeclID] = []
    if let r = ast[d].receiver {
      result.append(AnyDeclID(r))
    } else {
      result.append(contentsOf: ast[d].explicitCaptures.map(AnyDeclID.init(_:)))
      result.append(contentsOf: implicitCaptures[d]!.map(\.decl))
    }
    return result
  }

  /// Returns the declarations of `d`' captures.
  ///
  /// If `d` is a member subscript, its receiver is its only capture. Otherwise, its explicit
  /// captures come first, in the order they appear in its capture list, from left to right.
  /// Implicit captures come next, in the order they were found during type checking.
  public func captures(of d: SubscriptImpl.ID) -> [AnyDeclID] {
    var result: [AnyDeclID] = []
    if let r = ast[d].receiver {
      result.append(AnyDeclID(r))
    } else {
      let bundle = SubscriptDecl.ID(nodeToScope[d]!)!
      result.append(contentsOf: ast[bundle].explicitCaptures.map(AnyDeclID.init(_:)))
      result.append(contentsOf: implicitCaptures[bundle]!.map(\.decl))
    }
    return result
  }

  /// Returns the generic parameters captured by `useScope`, outer to inner, left to right.
  ///
  /// A declaration may take generic parameters even if it doesn't declare any. For example, a
  /// nested function will implicitly capture the generic parameters introduced in its context.
  public func accumulatedGenericParameters<S: ScopeID>(
    of useScope: S
  ) -> ReversedCollection<[GenericParameterDecl.ID]> {
    var parameters: [GenericParameterDecl.ID] = []
    accumulateGenericParameters(of: useScope, in: &parameters)
    return parameters.reversed()
  }

  /// Accumulates the generic parameters captured by `useScope` in `parameters`, inner to outer,
  /// right to left.
  private func accumulateGenericParameters<S: ScopeID>(
    of useScope: S, in parameters: inout [GenericParameterDecl.ID]
  ) {
    for s in scopes(from: useScope) {
      switch s.kind.value {
      case is ConformanceDecl.Type:
        if let p = scopeExtended(by: ConformanceDecl.ID(s)!) {
          accumulateGenericParameters(of: p, in: &parameters)
        }

      case is ExtensionDecl.Type:
        if let p = scopeExtended(by: ExtensionDecl.ID(s)!) {
          accumulateGenericParameters(of: p, in: &parameters)
        }

      case is GenericScope.Type:
        parameters.append(contentsOf: environment[AnyDeclID(s)!]!.parameters)

      case is TranslationUnit.Type:
        // No need to look further.
        return

      default:
        continue
      }
    }
  }

  /// Returns the scope of the declaration extended by `d`, if any.
  private func scopeExtended<T: TypeExtendingDecl>(by d: T.ID) -> AnyScopeID? {
    switch MetatypeType(declType[d])?.instance.base {
    case let u as ProductType:
      return AnyScopeID(u.decl)
    case let u as TypeAliasType:
      return AnyScopeID(u.decl)
    default:
      return nil
    }
  }

  /// Returns a copy of `generic` monomorphized for the given `parameterization`.
  public func monomorphize(_ generic: AnyType, for parameterization: GenericArguments) -> AnyType {
    relations.monomorphize(generic, for: parameterization)
  }

  /// Returns `arguments` monomorphized for the given `parameterization`.
  public func monomorphize(
    _ arguments: GenericArguments, for parameterization: GenericArguments
  ) -> GenericArguments {
    relations.monomorphize(arguments, for: parameterization)
  }

  /// If `t` has a record layout, returns the names and types of its stored properties, replacing
  /// generic parameters by their corresponding value in `parameterization`. Otherwise, returns an
  /// empty array.
  public func storage(
    of t: AnyType, parameterizedBy parameterization: GenericArguments = [:]
  ) -> [StoredProperty] {
    switch t.base {
    case let u as BoundGenericType:
      return storage(of: u.base, parameterizedBy: parameterization.appending(u.arguments))

    case let u as ProductType:
      return ast[u.decl].members.flatMap { (m) in
        BindingDecl.ID(m).map { (b) in
          ast.names(in: ast[b].pattern).map { (_, name) in
            let t = relations.monomorphize(declType[ast[name].decl]!, for: parameterization)
            return (ast[ast[name].decl].baseName, t)
          }
        } ?? []
      }

    case let u as TupleType:
      return u.elements.map({ (l) in
        let t = relations.monomorphize(l.type, for: parameterization)
        return (l.label, t)
      })

    default:
      assert(!t.hasRecordLayout)
      return []
    }
  }

  /// Returns `true` iff `model` conforms to `concept` in `useScope`.
  public func conforms(_ model: AnyType, to concept: TraitType, in useScope: AnyScopeID) -> Bool {
    conformance(of: model, to: concept, exposedTo: useScope) != nil
  }

  /// Returns the conformance of `model` to `Val.Deinitializable` exposed to `useScope` or `nil` if
  /// no such conformance exists.
  public func conformanceToDeinitializable(
    of model: AnyType, exposedTo useScope: AnyScopeID
  ) -> Conformance? {
    conformance(of: model, to: ast.deinitializableTrait, exposedTo: useScope)
  }

  /// Returns the conformance of `model` to `Val.Movable` exposed to `useScope` or `nil` if no such
  /// conformance exists.
  public func conformanceToMovable(
    of model: AnyType, exposedTo useScope: AnyScopeID
  ) -> Conformance? {
    conformance(of: model, to: ast.movableTrait, exposedTo: useScope)
  }

  /// Returns the conformance of `model` to `concept` that is exposed to `useScope`, or `nil` if
  /// such a conformance doesn't exist
  public func conformance(
    of model: AnyType, to concept: TraitType, exposedTo useScope: AnyScopeID
  ) -> Conformance? {
    let m = relations.canonical(model)

    if let c = declaredConformance(of: m, to: concept, exposedTo: useScope) {
      return c
    } else {
      return structuralConformance(of: m, to: concept, exposedTo: useScope)
    }
  }

  /// Returns the explicitly declared conformance of `model` to `concept` that is exposed to
  /// `useScope`, or `nil` if such a conformance doesn't exist.
  ///
  /// This method returns `nil` if the conformance of `model` to `concept` is structural (e.g., a
  /// tuple's synthesized conformance to `Movable`).
  private func declaredConformance(
    of model: AnyType, to concept: TraitType, exposedTo useScope: AnyScopeID
  ) -> Conformance? {
    assert(model[.isCanonical])

    // `A<X>: T` iff `A: T`.
    if let t = BoundGenericType(model) {
      guard let c = conformance(of: t.base, to: concept, exposedTo: useScope) else {
        return nil
      }

      // TODO: translate generic arguments to conditions

      return .init(
        model: t.base, concept: concept, arguments: t.arguments, conditions: [],
        scope: c.scope, implementations: c.implementations, isStructural: c.isStructural,
        site: c.site)
    }

    guard
      let allConformances = relations.conformances[model],
      let conformancesToConcept = allConformances[concept]
    else { return nil }

    // Return the first conformance exposed to `useSite`,
    let fileImports = imports[source(containing: useScope), default: []]
    return conformancesToConcept.first { (c) in
      if let m = ModuleDecl.ID(c.scope), fileImports.contains(m) {
        return true
      } else {
        return isContained(useScope, in: c.scope)
      }
    }
  }

  /// Returns the implicit structural conformance of `model` to `concept` that is exposed to
  /// `useScope`, or `nil` if such a conformance doesn't exist.
  private func structuralConformance(
    of model: AnyType, to concept: TraitType, exposedTo useScope: AnyScopeID
  ) -> Conformance? {
    assert(model[.isCanonical])

    switch model.base {
    case let m as TupleType:
      guard m.elements.allSatisfy({ conforms($0.type, to: concept, in: useScope) }) else {
        return nil
      }

    case let m as UnionType:
      guard m.elements.allSatisfy({ conforms($0, to: concept, in: useScope) }) else {
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
      let t = LambdaType(monomorphize(declType[requirement]!, for: a))!
      let d = SynthesizedFunctionDecl(k, typed: t, in: useScope)
      implementations[requirement] = .synthetic(d)
    }

    return .init(
      model: model, concept: concept, arguments: [:], conditions: [], scope: useScope,
      implementations: implementations, isStructural: true,
      site: .empty(at: ast[useScope].site.first()))
  }

}
