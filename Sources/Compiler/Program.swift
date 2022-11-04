/// A type representing a Val program and its properties at a some stage of compilation.
public protocol Program {

  /// The AST of the program.
  var ast: AST { get }

  /// A map from scope to its parent scope.
  var scopeToParent: ASTProperty<AnyScopeID> { get }
  // parent

  /// A map from scope to the declarations directly contained in them.
  var scopeToDecls: ASTProperty<[AnyDeclID]> { get }
  // containees

  /// A map from declaration to its scope.
  var declToScope: DeclProperty<AnyScopeID> { get }
  // container

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
    // Type declarations are global.
    if decl.kind <= .typeDecl {
      return true
    }

    // Declarations at global scope are global.
    switch declToScope[decl]!.kind {
    case .topLevelDeclSet, .namespaceDecl:
      return true

    default:
      break
    }

    // Static member declarations and initializers are global.
    switch decl.kind {
    case .bindingDecl:
      return ast[NodeID<BindingDecl>(rawValue: decl.rawValue)].isStatic

    case .funDecl:
      let i = NodeID<FunDecl>(rawValue: decl.rawValue)
      return (
        ast[i].isStatic
        || ast[i].introducer.value == .`init`
        || ast[i].introducer.value == .memberwiseInit)

    case .subscriptDecl:
      return ast[NodeID<SubscriptDecl>(rawValue: decl.rawValue)].isStatic

    default:
      return false
    }
  }

  /// Returns whether `decl` is member of a type declaration.
  public func isMember<T: DeclID>(_ decl: T) -> Bool {
    guard let parent = declToScope[decl] else { return false }
    switch parent.kind {
    case .productTypeDecl, .traitDecl, .conformanceDecl, .extensionDecl, .typeAliasDecl:
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
  public func isNonStaticMember(_ decl: NodeID<FunDecl>) -> Bool {
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
