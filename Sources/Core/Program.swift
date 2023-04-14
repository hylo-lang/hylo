import Utils

/// A type representing a Val program and its properties at a some stage of compilation.
public protocol Program {

  /// The AST of the program.
  var ast: AST { get }

  /// A map from scope to its parent scope.
  var scopeToParent: ASTProperty<AnyScopeID> { get }

  /// A map from scope to the declarations directly contained in them.
  var scopeToDecls: ASTProperty<[AnyDeclID]> { get }

  /// A map from declaration to the innermost scope that contains it.
  var declToScope: DeclProperty<AnyScopeID> { get }

  /// A map from name expression to the innermost scope that contains it.
  var exprToScope: [NameExpr.ID: AnyScopeID] { get }

  /// A map from variable declaration its containing binding declaration.
  var varToBinding: [VarDecl.ID: BindingDecl.ID] { get }

}

extension Program {

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
      } else if let p = scopeToParent[current] {
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
      return scopeToParent[declToScope[d]!]!
    case ModuleDecl.self:
      return AnyScopeID(ModuleDecl.ID(d)!)
    default:
      return declToScope[d]!
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
    switch declToScope[decl]!.kind {
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

  /// Returns whether `decl` is member of a type declaration.
  public func isMember<T: DeclID>(_ decl: T) -> Bool {
    guard let parent = declToScope[decl] else { return false }
    switch parent.kind {
    case ConformanceDecl.self,
      ExtensionDecl.self,
      ProductTypeDecl.self,
      TraitDecl.self,
      TypeAliasDecl.self:
      return true

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
  public func isNonStaticMember(_ decl: SubscriptDecl.ID) -> Bool {
    !ast[decl].isStatic && isMember(decl)
  }

  /// Returns whether `decl` is local in `ast`.
  public func isLocal<T: DeclID>(_ decl: T) -> Bool {
    !isGlobal(decl) && !isMember(decl)
  }

  /// Returns whether `decl` is a requirement.
  public func isRequirement<T: DeclID>(_ decl: T) -> Bool {
    switch decl.kind {
    case FunctionDecl.self, InitializerDecl.self, MethodDecl.self, SubscriptDecl.self:
      return declToScope[decl]!.kind == TraitDecl.self
    case MethodImpl.self:
      return isRequirement(MethodDecl.ID(declToScope[decl]!)!)
    case SubscriptImpl.self:
      return isRequirement(SubscriptDecl.ID(declToScope[decl]!)!)
    default:
      return false
    }
  }

  /// Returns whether `scope` denotes a member context.
  public func isMemberContext<S: ScopeID>(_ scope: S) -> Bool {
    switch scope.kind {
    case FunctionDecl.self:
      return isNonStaticMember(FunctionDecl.ID(scope)!)
    case SubscriptDecl.self:
      return isNonStaticMember(SubscriptDecl.ID(scope)!)
    case MethodDecl.self, InitializerDecl.self:
      return true
    case ModuleDecl.self:
      return false
    default:
      return isMemberContext(scopeToParent[scope]!)
    }
  }

  /// Returns a sequence containing `scope` and all its ancestors, from inner to outer.
  public func scopes<S: ScopeID>(from scope: S) -> LexicalScopeSequence {
    LexicalScopeSequence(scopeToParent: scopeToParent, from: scope)
  }

  /// Returns the innermost type scope containing `d`.
  public func innermostType<T: DeclID>(containing d: T) -> AnyScopeID? {
    scopes(from: declToScope[d]!).first(where: { $0.kind.value is TypeScope.Type })
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

  public func debugName<T: DeclID>(decl d: T) -> String {
    let s = (declToScope[d].map(debugName(scope:)) ?? "") + "."
    switch d.kind {
    case AssociatedTypeDecl.self:
      return s + ast[AssociatedTypeDecl.ID(d)!].baseName
    case AssociatedValueDecl.self:
      return s + ast[AssociatedValueDecl.ID(d)!].baseName
    case BindingDecl.self:
      return s + "\(d.rawValue)"
    case ConformanceDecl.self:
      return s + "\(d.rawValue)"
    case ExtensionDecl.self:
      return s + "\(d.rawValue)"
    case FunctionDecl.self:
      return s + (ast[FunctionDecl.ID(d)!].identifier?.value ?? "\(d.rawValue)") + "#\(d.rawValue)"
    case GenericParameterDecl.self:
      return s + ast[GenericParameterDecl.ID(d)!].baseName
    case ImportDecl.self:
      return s + ast[ImportDecl.ID(d)!].baseName
    case InitializerDecl.self:
      return s + "init#\(d.rawValue)"
    case MethodDecl.self:
      return s + ast[ImportDecl.ID(d)!].baseName + "#\(d.rawValue)"
    case MethodImpl.self:
      return s + String(describing: ast[MethodImpl.ID(d)!].introducer.value)
    case ModuleDecl.self:
      return ast[ModuleDecl.ID(d)!].baseName
    case NamespaceDecl.self:
      return s + ast[NamespaceDecl.ID(d)!].baseName
    case OperatorDecl.self:
      return s + ast[OperatorDecl.ID(d)!].name.value
    case ParameterDecl.self:
      return s + ast[ParameterDecl.ID(d)!].baseName
    case ProductTypeDecl.self:
      return s + ast[ProductTypeDecl.ID(d)!].baseName
    case SubscriptDecl.self:
      let n = ast[SubscriptDecl.ID(d)!].identifier?.value ?? "\(d.rawValue)"
      return s + "\(s)\(n)#\(d.rawValue)"
    case SubscriptImpl.self:
      return s + String(describing: ast[SubscriptImpl.ID(d)!].introducer.value)
    case TraitDecl.self:
      return s + ast[TraitDecl.ID(d)!].baseName
    case TypeAliasDecl.self:
      return s + ast[TypeAliasDecl.ID(d)!].baseName
    case VarDecl.self:
      return s + ast[VarDecl.ID(d)!].baseName
    default:
      unreachable()
    }
  }

  public func debugName<T: ScopeID>(scope s: T) -> String {
    if let d = AnyDeclID(s) {
      return debugName(decl: d)
    } else {
      let p = (scopeToParent[s].map(debugName(scope:)) ?? "") + "."
      return "\(p)\(s.rawValue)"
    }
  }

}
