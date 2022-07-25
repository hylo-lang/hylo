/// A graph describing the scope hierarchy of an AST.
public struct ScopeHierarchy {

  /// A sequence of scopes, from inner to outer.
  struct ScopeSequence: IteratorProtocol, Sequence {

    typealias Element = AnyScopeID

    fileprivate let parent: NodeMap<AnyScopeID>

    fileprivate var current: AnyScopeID?

    mutating func next() -> AnyScopeID? {
      guard let s = current else { return nil }
      current = parent[s]
      return s
    }

  }

  /// A table mapping a lexical scope to its parent.
  var parent = NodeMap<AnyScopeID>()

  /// A table mapping a declaration to the innermost lexical scope that contains it.
  private(set) var container = DeclMap<AnyScopeID>()

  /// A table mapping lexical scopes to the declarations directly contained in them.
  private(set) var containees = NodeMap<[AnyDeclID]>()

  /// A table mapping a variable declaration its containing binding declaration.
  var varToBinding: [NodeID<VarDecl>: NodeID<BindingDecl>] = [:]

  /// Inserts `decl` into `scope`.
  mutating func insert<T: DeclID>(decl: T, into scope: AnyScopeID) {
    let child = AnyDeclID(decl)

    if let parent = container[child] {
      if parent == scope {
        // The relation is already established, we're done.
        return
      } else {
        // Remove the existing edge scope container to containee.
        containees[scope]?.removeAll(where: { $0 == child })
      }
    }

    // Create the edges.
    container[child] = scope
    containees[scope, default: []].append(child)
  }

  /// Returns whether `child` is contained in `ancestor`.
  ///
  /// Scope containment is transitive and reflexive; this method returns `true` if:
  /// - `child == ancestor`, or
  /// - `parent[child] == ancestor`, or
  /// - `isContained(parent[child], ancestor)`.
  ///
  /// - Requires: `child` is the identifier of a scope in this hierarchy.
  func isContained<T: NodeIDProtocol, U: ScopeID>(
    _ child: T,
    in ancestor: U
  ) -> Bool {
    var current = AnyNodeID(child)
    while true {
      if ancestor.rawValue == current.rawValue {
        return true
      } else if let p = parent[current] {
        current = p.base
      } else {
        return false
      }
    }
  }

  /// Returns whether `decl` is global in `ast`.
  func isGlobal<T: DeclID>(decl: T, ast: AST) -> Bool {
    // Type declarations are global.
    if decl.kind <= .typeDecl {
      return true
    }

    // Declarations at global scope are global.
    switch container[decl]!.kind {
    case .sourceDeclSet, .namespaceDecl:
      return true
    default:
      break
    }

    // Static member declarations are global.
    switch decl.kind {
    case .bindingDecl:
      return ast[NodeID<BindingDecl>(unsafeRawValue: decl.rawValue)].isStatic
    case .funDecl:
      return ast[NodeID<FunDecl>(unsafeRawValue: decl.rawValue)].isStatic
    case .subscriptDecl:
      return ast[NodeID<SubscriptDecl>(unsafeRawValue: decl.rawValue)].isStatic
    default:
      return false
    }
  }

  /// Returns whether `decl` is member of a type declaration.
  func isMember<T: DeclID>(decl: T) -> Bool {
    guard let parent = container[decl] else { return false }
    switch parent.kind {
    case .productTypeDecl, .traitDecl, .typeAliasDecl:
      return true
    default:
      return false
    }
  }

  /// Returns whether `decl` is a non-static member of a type declaration in `ast`.
  func isNonStaticMember<T: DeclID>(decl: T, ast: AST) -> Bool {
    isMember(decl: decl) && !isGlobal(decl: decl, ast: ast)
  }

  /// Returns whether `decl` is local in `ast`.
  func isLocal<T: DeclID>(decl: T, ast: AST) -> Bool {
    !(isGlobal(decl: decl, ast: ast) || isMember(decl: decl))
  }

  /// Returns a sequence containing `scope` and all its ancestors, from inner to outer.
  func scopesToRoot<S: ScopeID>(from scope: S) -> ScopeSequence {
    ScopeSequence(parent: parent, current: AnyScopeID(scope))
  }

  /// Returns the module containing `scope`.
  func module<S: ScopeID>(containing scope: S) -> NodeID<ModuleDecl>? {
    var last = AnyScopeID(scope)
    while let parent = parent[last] {
      last = parent
    }
    return NodeID(converting: last)
  }

}
