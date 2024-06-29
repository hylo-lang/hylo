import Utils

/// Types representing Hylo programs at some stage after syntactic and scope analysis.
public protocol Program {

  /// The AST of the program.
  var ast: AST { get }

  /// A map from node to the innermost scope that contains it.
  var nodeToScope: ASTProperty<AnyScopeID> { get }

  /// A map from scope to the declarations that it contains.
  var scopeToDecls: ASTProperty<DeclIDs> { get }

  /// A map from variable declaration its containing binding declaration.
  var varToBinding: [VarDecl.ID: BindingDecl.ID] { get }

}

extension Program {

  /// Projects the node associated with `id` bundled with its extrinsic relationships.
  public subscript<T: NodeIDProtocol>(id: T) -> BundledNode<T, Self> {
    .init(id, in: self)
  }

  /// Returns `true` iff `d` is a module's entry function.
  public func isModuleEntry(_ d: FunctionDecl.ID) -> Bool {
    let s = nodeToScope[d]!
    let n = ast[d].identifier?.value
    return (s.kind == TranslationUnit.self) && isPublic(d) && (n == "main")
  }

  /// `true` iff the `Builtin` module is visible in `s`.
  public func builtinIsVisible(in s: AnyScopeID) -> Bool {
    ast[module(containing: s)].canAccessBuiltins
  }

  /// Returns whether `child` is contained in `ancestor`.
  ///
  /// Lexical scope containment is transitive and reflexive; this method returns `true` if:
  /// - `child == ancestor`, or
  /// - `parent[child] == ancestor`, or
  /// - `isContained(parent[child], ancestor)`.
  ///
  /// - Requires: `child` is the identifier of a scope in this hierarchy.
  public func isContained<T: NodeIDProtocol, U: ScopeID>(_ child: T, in ancestor: U) -> Bool {
    var current = AnyNodeID(child)
    while true {
      if ancestor.rawValue == current.rawValue {
        return true
      } else if let p = nodeToScope[current] {
        current = p.base
      } else {
        return false
      }
    }
  }

  /// Returns whether `l` and `r` are in the same module.
  public func areInSameModule<L: NodeIDProtocol, R: NodeIDProtocol>(_ l: L, _ r: R) -> Bool {
    if let m = ModuleDecl.ID(l) {
      return isContained(r, in: m)
    } else {
      return isContained(r, in: module(containing: nodeToScope[l]!))
    }
  }

  /// Returns whether `l` overlaps with `r`.
  public func areOverlapping(_ l: AnyScopeID, _ r: AnyScopeID) -> Bool {
    isContained(l, in: r) || isContained(r, in: l)
  }

  /// Returns `true` iff `l` is lexically enclosed in more scopes than `r`.
  public func hasMoreAncestors<T: NodeIDProtocol, U: NodeIDProtocol>(
    _ l: T, than r: U
  ) -> Bool {
    guard let s = nodeToScope[l] else { return false }
    guard let t = nodeToScope[r] else { return true }

    var a = scopes(from: s)
    var b = scopes(from: t)
    while a.next() != nil {
      if b.next() == nil { return true }
    }
    return false
  }

  /// Compares `lhs` and `rhs` in `scopeOfUse` and returns whether one lexically shadows the other.
  ///
  /// `lhs` is lexically deeper than `rhs` w.r.t. `scopeOfUse` if any of these statements hold:
  /// - `lhs` is declared in the module containing `scopeOfUse` and `rhs` isn't.
  /// - `lhs` and `rhs` are declared in module containing `scopeOfUse` and `lhs` has more ancestors
  ///   than `rhs`.
  public func compareLexicalDepth<T: NodeIDProtocol, U: NodeIDProtocol>(
    _ lhs: T, _ rhs: U, in scopeOfUse: AnyScopeID
  ) -> StrictPartialOrdering {
    let m = module(containing: scopeOfUse)
    if isContained(lhs, in: m) {
      // If `lhs` is in the same module as `scopeOfUse` but `rhs` isn't, then `lhs` shadows `rhs`.
      guard isContained(rhs, in: m) else { return .ascending }

      // If `lhs` and `rhs` are in the same module as `scopeOfUse`, then `lhs` shadows `rhs` iff
      // it has more ancestors than `rhs`.
      if hasMoreAncestors(lhs, than: rhs) { return .ascending }
      if hasMoreAncestors(rhs, than: lhs) { return .descending }
      return nil
    }

    if isContained(rhs, in: m) {
      // If `rhs` is in the same module as `scopeOfUse` but `lhs` isn't, then `rhs` shadows `lhs`.
      return .descending
    }

    return nil
  }

  /// Returns the scope of `d`'s body, if any.
  public func scopeContainingBody(of d: FunctionDecl.ID) -> AnyScopeID? {
    switch ast[d].body {
    case nil:
      return nil
    case .some(.block(let s)):
      return AnyScopeID(s)
    case .some(.expr):
      return AnyScopeID(d)
    }
  }

  /// Returns the scope of `d`'s body, if any.
  public func scopeContainingBody(of d: MethodImpl.ID) -> AnyScopeID? {
    switch ast[d].body {
    case nil:
      return nil
    case .some(.block(let s)):
      return AnyScopeID(s)
    case .some(.expr):
      return AnyScopeID(d)
    }
  }

  /// Returns the scope of `d`'s body, if any.
  public func scopeContainingBody(of d: SubscriptImpl.ID) -> AnyScopeID? {
    switch ast[d].body {
    case nil:
      return nil
    case .some(.block(let s)):
      return AnyScopeID(s)
    case .some(.expr):
      return AnyScopeID(d)
    }
  }

  /// Returns `true` iff `s` is the body of a function, initializer, or subscript.
  public func isCallableBody(_ s: BraceStmt.ID) -> Bool {
    AnyDeclID(nodeToScope[s]!)?.isCallable ?? false
  }

  /// Returns `d` and the parameters and explicit captures introduced in the same scope.
  public func siblings(of d: ParameterDecl.ID) -> [AnyDeclID] {
    parametersAndCaptures(inBodyOf: AnyDeclID(nodeToScope[d]!)!)
  }

  /// Returns the parameters and explicit captures introduced in the body of `d`.
  private func parametersAndCaptures(inBodyOf d: AnyDeclID) -> [AnyDeclID] {
    let cs: [BindingDecl.ID]
    let ps: [ParameterDecl.ID]

    switch d.kind {
    case FunctionDecl.self:
      let f = FunctionDecl.ID(d)!
      (cs, ps) = (ast[f].explicitCaptures, ast[f].parameters)
    case SubscriptDecl.self:
      let f = SubscriptDecl.ID(d)!
      (cs, ps) = (ast[f].explicitCaptures, ast[f].parameters)
    case InitializerDecl.self:
      (cs, ps) = ([], ast[InitializerDecl.ID(d)!].parameters)
    case MethodDecl.self:
      (cs, ps) = ([], ast[MethodDecl.ID(d)!].parameters)
    case MethodImpl.self, SubscriptImpl.self:
      return parametersAndCaptures(inBodyOf: AnyDeclID(nodeToScope[d]!)!)
    default:
      return []
    }

    var r = ps.map(AnyDeclID.init(_:))
    for b in cs {
      for (_, n) in ast.names(in: ast[b].pattern) {
        r.append(AnyDeclID(ast[n].decl))
      }
    }
    return r
  }

  /// Returns `true` iff `d` is at module scope.
  public func isAtModuleScope<T: DeclID>(_ d: T) -> Bool {
    switch nodeToScope[d]!.kind {
    case TranslationUnit.self, NamespaceDecl.self:
      return true
    default:
      return false
    }
  }

  /// Returns whether `decl` is global.
  ///
  /// A declaration is global if and only if:
  /// - it is a type declaration; or
  /// - it is declared at global or namespace scope; or
  /// - it is declared at type scope and it is static; or
  /// - it is introduced with `init` or `memberwise init`.
  public func isGlobal<T: DeclID>(_ decl: T) -> Bool {
    switch decl.kind {
    case AssociatedTypeDecl.self,
      ImportDecl.self,
      ModuleDecl.self,
      NamespaceDecl.self,
      ProductTypeDecl.self,
      TraitDecl.self,
      TypeAliasDecl.self:
      // Type declarations are global.
      return true

    case GenericParameterDecl.self:
      // Generic parameters are global.
      return true

    default:
      break
    }

    // Declarations at global scope are global.
    switch nodeToScope[decl]!.kind {
    case TranslationUnit.self, NamespaceDecl.self:
      return true
    default:
      break
    }

    // Static member declarations and initializers are global.
    switch decl.kind {
    case BindingDecl.self:
      return ast[BindingDecl.ID(decl)!].isStatic
    case FunctionDecl.self:
      return ast[FunctionDecl.ID(decl)!].isStatic
    case InitializerDecl.self:
      return true
    case SubscriptDecl.self:
      return ast[SubscriptDecl.ID(decl)!].isStatic
    case VarDecl.self:
      return isGlobal(varToBinding[VarDecl.ID(decl)!]!)
    default:
      return false
    }
  }

  /// Returns whether `d` is member of a type declaration.
  public func isMember<T: DeclID>(_ d: T) -> Bool {
    guard let parent = nodeToScope[d] else { return false }

    switch parent.kind {
    case ConformanceDecl.self,
      ExtensionDecl.self,
      ProductTypeDecl.self,
      TraitDecl.self,
      TypeAliasDecl.self:
      return true

    case MethodDecl.self:
      return d.kind == MethodImpl.self

    case SubscriptDecl.self:
      return (d.kind == SubscriptImpl.self) && isMember(SubscriptDecl.ID(parent)!)

    default:
      return false
    }
  }

  /// Returns whether `decl` is a non-static member of a type declaration.
  public func isNonStaticMember<T: DeclID>(_ decl: T) -> Bool {
    !isGlobal(decl) && isMember(decl)
  }

  /// Returns whether `decl` is a non-static member of a type declaration.
  public func isNonStaticMember(_ decl: FunctionDecl.ID) -> Bool {
    !ast[decl].isStatic && isMember(decl)
  }

  /// Returns whether `decl` is a non-static member of a type declaration.
  public func isNonStaticMember(_ decl: MethodDecl.ID) -> Bool {
    true
  }

  /// Returns whether `decl` is a non-static member of a type declaration.
  public func isNonStaticMember(_ decl: SubscriptDecl.ID) -> Bool {
    !ast[decl].isStatic && isMember(decl)
  }

  /// Returns whether `decl` is local in `ast`.
  public func isLocal<T: DeclID>(_ decl: T) -> Bool {
    !isGlobal(decl) && !isMember(decl)
  }

  /// Returns `true` iff `d` is public.
  public func isPublic<T: DeclID>(_ d: T) -> Bool {
    switch d.kind {
    case SubscriptImpl.self:
      return isPublic(SubscriptDecl.ID(nodeToScope[d]!)!)
    case MethodImpl.self:
      return isPublic(MethodDecl.ID(nodeToScope[d]!)!)
    case VarDecl.self:
      return isPublic(varToBinding[.init(d)!]!)
    default:
      return (ast[d] as? ExposableDecl)?.accessModifier.value == .public
    }
  }

  /// Returns `true` iff `d` has the appropriate visibility when used as the implementation of a
  /// trait requirement.
  public func isAccessibleAsRequirementImplementation<T: DeclID>(_ d: T) -> Bool {
    (d.kind == GenericParameterDecl.self) || isPublic(d) || isRequirement(d)
  }

  /// Returns `true` iff `d` is visible outside of its module.
  ///
  /// - Note: modules are considered exported.
  public func isExported<T: DeclID>(_ d: T) -> Bool {
    (d.kind == ModuleDecl.self) || (isPublic(d) && isExportingDecls(nodeToScope[d]!))
  }

  /// Returns `true` iff the public declarations in `s` are visible outside of their module.
  public func isExportingDecls(_ s: AnyScopeID) -> Bool {
    switch s.kind {
    case ConformanceDecl.self:
      return isExported(ConformanceDecl.ID(s)!)
    case ExtensionDecl.self:
      return isExported(ExtensionDecl.ID(s)!)
    case MethodDecl.self:
      return isExportingDecls(nodeToScope[s]!)
    case ModuleDecl.self:
      return true
    case NamespaceDecl.self:
      return isExported(NamespaceDecl.ID(s)!)
    case ProductTypeDecl.self:
      return isExported(ProductTypeDecl.ID(s)!)
    case SubscriptDecl.self:
      return isExportingDecls(nodeToScope[s]!)
    case TraitDecl.self:
      return isExported(TraitDecl.ID(s)!)
    case TypeAliasDecl.self:
      return isExported(TypeAliasDecl.ID(s)!)
    case TranslationUnit.self:
      return true
    default:
      return false
    }
  }

  /// Returns `true` iff `d` is defined in an extension.
  public func isDefinedInExtension<T: DeclID>(_ d: T) -> Bool {
    switch d.kind {
    case ModuleDecl.self:
      return false
    case MethodImpl.self:
      return isDefinedInExtension(MethodDecl.ID(nodeToScope[d]!)!)
    case SubscriptImpl.self:
      return isDefinedInExtension(SubscriptDecl.ID(nodeToScope[d]!)!)
    default:
      return nodeToScope[d]!.kind == ExtensionDecl.self
    }
  }

  /// Returns `true` iff `d` is a trait requirement.
  public func isRequirement<T: DeclID>(_ d: T) -> Bool {
    switch d.kind {
    case AssociatedTypeDecl.self, AssociatedValueDecl.self:
      return true
    case FunctionDecl.self, InitializerDecl.self, MethodDecl.self, SubscriptDecl.self:
      return nodeToScope[d]!.kind == TraitDecl.self
    case MethodImpl.self:
      return isRequirement(MethodDecl.ID(nodeToScope[d]!)!)
    case SubscriptImpl.self:
      return isRequirement(SubscriptDecl.ID(nodeToScope[d]!)!)
    default:
      return false
    }
  }

  /// Returns `true` iff `d` declares a value usable as an implicit parameter.
  public func isImplicitDefinition(_ d: AnyDeclID) -> Bool {
    if let i = VarDecl.ID(d) {
      return ast[varToBinding[i]]!.isGiven
    } else if let i = ParameterDecl.ID(d) {
      return ast[i].isImplicit
    } else {
      return false
    }
  }

  /// If `s` is in a member context, returns the innermost receiver declaration exposed to `s`.
  /// Otherwise, returns `nil`
  public func innermostReceiver(in useScope: AnyScopeID) -> ParameterDecl.ID? {
    switch useScope.kind {
    case FunctionDecl.self:
      if let d = ast[FunctionDecl.ID(useScope)!].receiver { return d }
    case InitializerDecl.self:
      return ast[InitializerDecl.ID(useScope)!].receiver
    case MethodImpl.self:
      return ast[MethodImpl.ID(useScope)!].receiver
    case ModuleDecl.self:
      return nil
    case SubscriptImpl.self:
      if let d = ast[SubscriptImpl.ID(useScope)!].receiver { return d }
    default:
      break
    }

    return innermostReceiver(in: nodeToScope[useScope]!)
  }

  /// Returns a sequence containing `scope` and all its ancestors, from inner to outer.
  public func scopes<S: ScopeID>(from scope: S) -> LexicalScopeSequence {
    LexicalScopeSequence(scopeToParent: nodeToScope, from: scope)
  }

  /// Returns the innermost scope that is a common ancestor of `a` and `b` or `nil` if `a` and `b`
  /// are in different modules.
  public func innermostCommonScope(_ a: AnyScopeID, _ b: AnyScopeID) -> AnyScopeID? {
    let x = scopes(from: a).reversed()
    let y = scopes(from: b).reversed()

    var result: AnyScopeID?
    for i in x.indices {
      if (i == y.count) || (x[i] != y[i]) { break }
      result = x[i]
    }
    return result
  }

  /// Returns the module containing `scope`.
  public func module<S: ScopeID>(containing scope: S) -> ModuleDecl.ID {
    scopes(from: scope).first(ModuleDecl.self)!
  }

  /// Returns the translation unit containing `scope`.
  ///
  /// - Requires:`scope` is not a module.
  public func source<S: ScopeID>(containing scope: S) -> TranslationUnit.ID {
    scopes(from: scope).first(TranslationUnit.self)!
  }

  /// Returns the name of `d`.
  public func name<T: DeclID>(of d: T) -> Name? {
    if let e = self.ast[d] as? SingleEntityDecl { return Name(stem: e.baseName) }

    switch d.kind {
    case FunctionDecl.self:
      return ast.name(of: FunctionDecl.ID(d)!)
    case InitializerDecl.self:
      return ast.name(of: InitializerDecl.ID(d)!)
    case MethodImpl.self:
      return ast.name(of: MethodDecl.ID(self[d].scope)!)
    case SubscriptImpl.self:
      return ast.name(of: SubscriptDecl.ID(self[d].scope)!)
    default:
      return nil
    }
  }

  /// Returns `(root: r, path: p)` where `root` is the binding declaration introducing `d` and `p`
  /// is the path to the object bound to `d` relative to `root`.
  public func subfieldRelativeToRoot(
    of d: VarDecl.ID
  ) -> (root: BindingDecl.ID, path: RecordPath) {
    let root = varToBinding[d]!
    for (p, n) in ast.names(in: ast[root].pattern) {
      if ast[n].decl == d { return (root, p) }
    }
    unreachable()
  }

  /// Returns the declarations of the stored part of `p`.
  public func storedParts(of p: ProductTypeDecl.ID) -> [VarDecl.ID] {
    var result: [VarDecl.ID] = []
    for m in ast[p].members.filter(BindingDecl.self) {
      for (_, n) in ast.names(in: ast[m].pattern) {
        result.append(self[n].decl)
      }
    }
    return result
  }

  /// Returns a textual description of `n` suitable for debugging.
  public func debugDescription<T: NodeIDProtocol>(_ n: T) -> String {
    switch n.kind {
    case BindingDecl.self:
      return debugDescription(BindingDecl.ID(n)!)
    case ModuleDecl.self:
      return ast[ModuleDecl.ID(n)!].baseName
    default:
      break
    }

    let qualification = debugDescription(nodeToScope[n]!)

    if let d = AnyDeclID(n) {
      if let s = name(of: d) {
        return qualification + ".\(s)"
      } else if d.kind == FunctionDecl.self {
        return qualification + ".lambda"
      }
    }

    return qualification
  }

  /// Returns a textual description of `d` suitable for debugging.
  public func debugDescription(_ d: BindingDecl.ID) -> String {
    let ns = ast.names(in: self[d].pattern.subpattern)
      .map({ ast[ast[$0.pattern].decl].baseName })
      .joined()
    return debugDescription(nodeToScope[d]!) + ".(\(ns))"
  }

}
