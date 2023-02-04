/// A type representing a Val program and its properties at a some stage of compilation.
public protocol Program {

  /// The AST of the program.
  var ast: AST { get }

  /// A map from scope to its parent scope.
  var scopeToParent: ASTProperty<AnyScopeID> { get }

  /// A map from scope to the declarations directly contained in them.
  var scopeToDecls: ASTProperty<[AnyDeclID]> { get }

  /// A map from declaration to its scope.
  var declToScope: DeclProperty<AnyScopeID> { get }

  /// A map from variable declaration its containing binding declaration.
  var varToBinding: [NodeID<VarDecl>: NodeID<BindingDecl>] { get }

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
      return ast[NodeID<BindingDecl>(decl)!].isStatic

    case FunctionDecl.self:
      return ast[NodeID<FunctionDecl>(decl)!].isStatic

    case InitializerDecl.self:
      return true

    case SubscriptDecl.self:
      return ast[NodeID<SubscriptDecl>(decl)!].isStatic

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
  public func isNonStaticMember(_ decl: NodeID<FunctionDecl>) -> Bool {
    !ast[decl].isStatic && isMember(decl)
  }

  /// Returns whether `decl` is a non-static member of a type declaration.
  public func isNonStaticMember(_ decl: NodeID<SubscriptDecl>) -> Bool {
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
      return isRequirement(NodeID<MethodDecl>(declToScope[decl]!)!)
    case SubscriptImpl.self:
      return isRequirement(NodeID<SubscriptDecl>(declToScope[decl]!)!)
    default:
      return false
    }
  }

  /// Returns whether `scope` denotes a member context.
  public func isMemberContext<S: ScopeID>(_ scope: S) -> Bool {
    switch scope.kind {
    case FunctionDecl.self:
      return isNonStaticMember(NodeID<FunctionDecl>(scope)!)

    case SubscriptDecl.self:
      return isNonStaticMember(NodeID<SubscriptDecl>(scope)!)

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
    LexicalScopeSequence(scopeToParent: scopeToParent, current: AnyScopeID(scope))
  }

  /// Returns the module containing `scope`.
  public func module<S: ScopeID>(containing scope: S) -> NodeID<ModuleDecl>? {
    var last = AnyScopeID(scope)
    while let parent = scopeToParent[last] {
      last = parent
    }
    return NodeID(last)
  }

}
