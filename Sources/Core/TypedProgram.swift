/// A data structure representing a typed Val program ready to be lowered.
public struct TypedProgram: Program {

  public let ast: AST

  public let scopeToParent: ASTProperty<AnyScopeID>

  public let scopeToDecls: ASTProperty<[AnyDeclID]>

  public let declToScope: DeclProperty<AnyScopeID>

  public let exprToScope: [NameExpr.ID: AnyScopeID]

  public let varToBinding: [VarDecl.ID: BindingDecl.ID]

  /// A map from translation unit to its imports.
  public let imports: [TranslationUnit.ID: Set<ModuleDecl.ID>]

  /// The overarching type of each declaration.
  public let declTypes: DeclProperty<AnyType>

  /// The type of each expression.
  public let exprTypes: ExprProperty<AnyType>

  /// A map from function and subscript declarations to their implicit captures.
  public let implicitCaptures: DeclProperty<[ImplicitCapture]>

  /// A map from generic declarations to their environment.
  public let environments: DeclProperty<GenericEnvironment>

  /// A map from module to its synthesized declarations.
  public let synthesizedDecls: [ModuleDecl.ID: [SynthesizedDecl]]

  /// A map from name expression to its referred declaration.
  public let referredDecls: [NameExpr.ID: DeclReference]

  /// A map from sequence expressions to their evaluation order.
  public let foldedSequenceExprs: [SequenceExpr.ID: FoldedSequenceExpr]

  /// The type relations of the program.
  public let relations: TypeRelations

  /// Val's core library.
  public var coreLibrary: ModuleDecl.Typed? {
    ast.coreLibrary.map({ self[$0] })
  }

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
    synthesizedDecls: [ModuleDecl.ID: [SynthesizedDecl]],
    referredDecls: [NameExpr.ID: DeclReference],
    foldedSequenceExprs: [SequenceExpr.ID: FoldedSequenceExpr],
    relations: TypeRelations
  ) {
    precondition(program.ast.modules.allSatisfy({ declTypes[$0]?.base is ModuleType }))

    self.ast = program.ast
    self.scopeToParent = program.scopeToParent
    self.scopeToDecls = program.scopeToDecls
    self.declToScope = program.declToScope
    self.exprToScope = program.exprToScope
    self.varToBinding = program.varToBinding
    self.imports = imports
    self.declTypes = declTypes
    self.exprTypes = exprTypes
    self.implicitCaptures = implicitCaptures
    self.environments = environments
    self.synthesizedDecls = synthesizedDecls
    self.referredDecls = referredDecls
    self.foldedSequenceExprs = foldedSequenceExprs
    self.relations = relations
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
      let bundle = SubscriptDecl.ID(declToScope[d]!)!
      result.append(contentsOf: ast[bundle].explicitCaptures.map(AnyDeclID.init(_:)))
      result.append(contentsOf: implicitCaptures[bundle]!.map(\.decl))
    }
    return result
  }

  /// Returns the generic parameters taken by `s`, in outer to inner, left to right.
  ///
  /// A declaration may take generic parameters even if it doesn't declare any. For example, a
  /// nested function will implicitly capture the generic parameters introduced in its context.
  public func accumulatedGenericParameters<S: ScopeID>(
    of s: S
  ) -> some Collection<GenericParameterDecl.ID> {
    let p = scopes(from: s).compactMap { (t) -> [GenericParameterDecl.ID]? in
      if !(t.kind.value is GenericScope.Type) { return nil }
      return environments[AnyDeclID(t)!]!.parameters
    }
    return p.reversed().joined()
  }

  /// Returns a copy of `generic` monomorphized for the given `arguments`.
  ///
  /// This method has no effect if `arguments` is empty.
  public func monomorphize(_ generic: AnyType, for arguments: GenericArguments) -> AnyType {
    relations.monomorphize(generic, for: arguments)
  }

  /// If `t` has a record layout, returns the names and types of its stored properties. Otherwise,
  /// returns an empty array.
  public func storage(of t: AnyType) -> [StoredProperty] {
    switch t.base {
    case let u as BoundGenericType:
      return storage(of: u.base)

    case let u as ProductType:
      return self[u.decl].members.flatMap { (m) in
        BindingDecl.Typed(m).map { (b) in
          b.pattern.names.lazy.map({ (_, name) in (name.decl.baseName, name.decl.type) })
        } ?? []
      }

    case let u as TupleType:
      return u.elements.map({ ($0.label, $0.type) })

    default:
      assert(!t.hasRecordLayout)
      return []
    }
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

  /// Returns the conformance of `model` to `concept` exposed to `useScope` or `nil` if no such
  /// conformance exists.
  public func conformance(
    of model: AnyType, to concept: TraitType, exposedTo useScope: AnyScopeID
  ) -> Conformance? {
    guard
      let allConformances = relations.conformances[relations.canonical(model)],
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

}
