import Utils

/// A data structure representing a scoped Val program ready to be type checked.
public struct ScopedProgram: Program {

  public let ast: AST

  public let scopeToParent: ASTProperty<AnyScopeID>

  public let scopeToDecls: ASTProperty<[AnyDeclID]>

  public let declToScope: DeclProperty<AnyScopeID>

  public let varToBinding: [VarDecl.ID: BindingDecl.ID]

  /// Creates a scoped program from an AST.
  public init(_ ast: AST) {
    // Establish the scope relationships.
    var s = ScopeVisitor()
    for m in ast.modules {
      ast.walk(m, notifying: &s)
    }

    self.ast = ast
    self.scopeToParent = s.scopeToParent
    self.scopeToDecls = s.scopeToDecls
    self.declToScope = s.declToScope
    self.varToBinding = s.varToBinding
  }

}

/// The state of the visitor building scope relationships.
private struct ScopeVisitor: ASTWalkObserver {

  /// A map from scope to its parent scope.
  var scopeToParent = ASTProperty<AnyScopeID>()

  /// A map from scope to the declarations directly contained in them.
  var scopeToDecls = ASTProperty<[AnyDeclID]>()

  /// A map from declaration to its scope.
  var declToScope = DeclProperty<AnyScopeID>()

  /// A map from variable declaration its containing binding declaration.
  var varToBinding: [VarDecl.ID: BindingDecl.ID] = [:]

  /// A stack containing the bindind declarations currently visited.
  var bindingDecls: [BindingDecl.ID] = []

  /// The innermost lexical scope currently visited.
  var innermost: AnyScopeID?

  /// Inserts `child` into `scope`.
  private mutating func insert(child: AnyDeclID, into scope: AnyScopeID) {
    // FIXME: precondition(child.kind != ModuleDecl.self)
    if let parent = declToScope[child] {
      if parent == scope {
        // The relation is already established, we're done.
        return
      } else {
        // Remove the existing edge scope container to containee.
        scopeToDecls[scope]?.removeAll(where: { $0 == child })
      }
    }

    // Create the edges.
    declToScope[child] = scope
    scopeToDecls[scope, default: []].append(child)
  }

  mutating func willEnter(_ n: AnyNodeID, in ast: AST) -> Bool {
    switch n.kind {
    case BindingDecl.self:
      return visit(bindingDecl: NodeID(n)!, in: ast)
    case ModuleDecl.self:
      return visit(moduleDecl: NodeID(n)!, in: ast)
    case VarDecl.self:
      return visit(varDecl: NodeID(n)!, in: ast)
    case ConditionalExpr.self:
      return visit(conditionalExpr: NodeID(n)!, in: ast)
    case ConditionalStmt.self:
      return visit(conditionalStmt: NodeID(n)!, in: ast)
    case DoWhileStmt.self:
      return visit(doWhileStmt: NodeID(n)!, in: ast)
    default:
      break
    }

    if let d = AnyDeclID(n) {
      insert(child: d, into: innermost!)
    }
    if let s = AnyScopeID(n) {
      scopeToParent[s] = innermost
      innermost = s
    }

    return true
  }

  mutating func willExit(_ n: AnyNodeID, in ast: AST) {
    if let d = BindingDecl.ID(n) {
      let x = bindingDecls.removeLast() == d
      assert(x)
    }
    if let s = AnyScopeID(n) {
      innermost = scopeToParent[s]!
    }
  }

  private mutating func visit(bindingDecl d: BindingDecl.ID, in ast: AST) -> Bool {
    insert(child: AnyDeclID(d), into: innermost!)
    bindingDecls.append(d)
    ast.traverse(ast[d], notifying: &self)
    bindingDecls.removeLast()
    return false
  }

  private mutating func visit(moduleDecl d: ModuleDecl.ID, in ast: AST) -> Bool {
    assert(innermost == nil)
    innermost = AnyScopeID(d)
    ast.traverse(ast[d], notifying: &self)
    innermost = nil
    return false
  }

  private mutating func visit(varDecl d: VarDecl.ID, in ast: AST) -> Bool {
    insert(child: AnyDeclID(d), into: innermost!)
    // FIXME: incorrect if we're in a match case
    varToBinding[d] = bindingDecls.last
    return false
  }

  private mutating func visit(conditionalExpr e: ConditionalExpr.ID, in ast: AST) -> Bool {
    scopeToParent[e] = innermost
    innermost = AnyScopeID(e)
    ast.walk(conditionItems: ast[e].condition, notifying: &self)
    ast.walk(ast[e].success, notifying: &self)

    // The failure branch is not in the scope of the conditional expression.
    innermost = scopeToParent[e]!
    ast.walk(ast[e].failure, notifying: &self)
    return false
  }

  private mutating func visit(conditionalStmt s: ConditionalStmt.ID, in ast: AST) -> Bool {
    scopeToParent[s] = innermost
    innermost = AnyScopeID(s)
    ast.walk(conditionItems: ast[s].condition, notifying: &self)
    ast.walk(ast[s].success, notifying: &self)

    // The failure branch is not in the scope of the conditional expression.
    innermost = scopeToParent[s]!
    ast.walk(ast[s].failure, notifying: &self)
    return false
  }

  private mutating func visit(doWhileStmt s: DoWhileStmt.ID, in ast: AST) -> Bool {
    scopeToParent[ast[s].body] = innermost
    innermost = AnyScopeID(ast[s].body)
    ast.walk(ast[ast[s].body].stmts, notifying: &self)

    // The condition is in the same scope as the body.
    ast.walk(ast[s].condition, notifying: &self)
    innermost = scopeToParent[ast[s].body]!
    return false
  }

}
