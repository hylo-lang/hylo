import Utils

/// Types representing Val programs at some stage after syntactic and scope analysis.
public protocol Program {

  /// The AST of the program.
  var ast: AST { get }

  /// A map from node to the innermost scope that contains it.
  var nodeToScope: ASTProperty<AnyScopeID> { get }

  /// A map from scope to the declarations directly contained in them.
  var scopeToDecls: ASTProperty<[AnyDeclID]> { get }

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

  /// Returns whether `child` is contained in `ancestor`.
  ///
  /// Lexical scope containment is transitive and reflexive; this method returns `true` if:
  /// - `child == ancestor`, or
  /// - `parent[child] == ancestor`, or
  /// - `isContained(parent[child], ancestor)`.
  ///
  /// - Requires: `child` is the identifier of a scope in this hierarchy.
  public func isContained<T: NodeIDProtocol, U: ScopeID>(
    _ child: T,
    in ancestor: U
  ) -> Bool {
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

  /// Returns whether `l` overlaps with `r`.
  public func areOverlapping(_ l: AnyScopeID, _ r: AnyScopeID) -> Bool {
    isContained(l, in: r) || isContained(r, in: l)
  }

  /// Returns the scope introducing `d`.
  public func scopeIntroducing(_ d: AnyDeclID) -> AnyScopeID {
    switch d.kind {
    case InitializerDecl.self:
      return scopeIntroducing(initializer: .init(d)!)
    case ModuleDecl.self:
      return AnyScopeID(ModuleDecl.ID(d)!)
    default:
      return nodeToScope[d]!
    }
  }

  /// Returns the scope introducing `d`.
  public func scopeIntroducing(initializer d: InitializerDecl.ID) -> AnyScopeID {
    nodeToScope[nodeToScope[d]!]!
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
    case BindingDecl.self:
      return ast[BindingDecl.ID(d)!].accessModifier.value == .public
    case ConformanceDecl.self:
      return ast[ConformanceDecl.ID(d)!].accessModifier.value == .public
    case ExtensionDecl.self:
      return ast[ExtensionDecl.ID(d)!].accessModifier.value == .public
    case FunctionDecl.self:
      return ast[FunctionDecl.ID(d)!].accessModifier.value == .public
    case InitializerDecl.self:
      return ast[InitializerDecl.ID(d)!].accessModifier.value == .public
    case MethodDecl.self:
      return ast[MethodDecl.ID(d)!].accessModifier.value == .public
    case NamespaceDecl.self:
      return ast[NamespaceDecl.ID(d)!].accessModifier.value == .public
    case OperatorDecl.self:
      return ast[OperatorDecl.ID(d)!].accessModifier.value == .public
    case ProductTypeDecl.self:
      return ast[ProductTypeDecl.ID(d)!].accessModifier.value == .public
    case SubscriptDecl.self:
      return ast[SubscriptDecl.ID(d)!].accessModifier.value == .public
    case TraitDecl.self:
      return ast[TraitDecl.ID(d)!].accessModifier.value == .public
    case TypeAliasDecl.self:
      return ast[TypeAliasDecl.ID(d)!].accessModifier.value == .public
    case VarDecl.self:
      return isPublic(varToBinding[.init(d)!]!)
    default:
      return false
    }
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
    case ModuleDecl.self:
      return true
    case NamespaceDecl.self:
      return isExported(NamespaceDecl.ID(s)!)
    case ProductTypeDecl.self:
      return isExported(ProductTypeDecl.ID(s)!)
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

  /// Returns whether `decl` is a requirement.
  public func isRequirement<T: DeclID>(_ decl: T) -> Bool {
    switch decl.kind {
    case AssociatedTypeDecl.self, AssociatedValueDecl.self:
      return true
    case FunctionDecl.self, InitializerDecl.self, MethodDecl.self, SubscriptDecl.self:
      return nodeToScope[decl]!.kind == TraitDecl.self
    case MethodImpl.self:
      return isRequirement(MethodDecl.ID(nodeToScope[decl]!)!)
    case SubscriptImpl.self:
      return isRequirement(SubscriptDecl.ID(nodeToScope[decl]!)!)
    default:
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

  /// Returns the name of `d` if it introduces a single entity.
  public func name(of d: AnyDeclID) -> Name? {
    if let e = self.ast[d] as? SingleEntityDecl { return Name(stem: e.baseName) }

    switch d.kind {
    case FunctionDecl.self:
      return ast.name(of: FunctionDecl.ID(d)!)!
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

  /// Returns a textual description of `n` suitable for debugging.
  public func debugDescription<T: NodeIDProtocol>(_ n: T) -> String {
    if let d = ModuleDecl.ID(n) {
      return ast[d].baseName
    }

    let qualification = debugDescription(nodeToScope[n]!)

    switch n.kind {
    case FunctionDecl.self:
      let s = ast.name(of: FunctionDecl.ID(n)!) ?? "lambda"
      return qualification + ".\(s)"
    case InitializerDecl.self:
      let s = ast.name(of: InitializerDecl.ID(n)!)
      return qualification + ".\(s)"
    case MethodDecl.self:
      let s = ast.name(of: MethodDecl.ID(n)!)
      return qualification + ".\(s)"
    case MethodImpl.self:
      let s = ast[MethodImpl.ID(n)!].introducer.value
      return qualification + ".\(s)"
    case SubscriptDecl.self:
      let s = ast.name(of: SubscriptDecl.ID(n)!)
      return qualification + ".\(s)"
    case SubscriptImpl.self:
      let s = ast[SubscriptImpl.ID(n)!].introducer.value
      return qualification + ".\(s)"
    default:
      break
    }

    if let e = ast[n] as? SingleEntityDecl {
      return qualification + "." + e.baseName
    } else {
      return qualification
    }
  }

}
