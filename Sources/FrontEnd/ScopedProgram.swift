import Utils

/// A data structure representing a scoped Hylo program ready to be type checked.
public struct ScopedProgram: Program {

  public let ast: AST

  public let nodeToScope: ASTProperty<AnyScopeID>

  public let scopeToDecls: ASTProperty<DeclIDs>

  public let varToBinding: [VarDecl.ID: BindingDecl.ID]

  /// Creates an instance with the given properties.
  private init(
    ast: AST,
    nodeToScope: ASTProperty<AnyScopeID>,
    scopeToDecls: ASTProperty<DeclIDs>,
    varToBinding: [VarDecl.ID: BindingDecl.ID]
  ) {
    self.ast = ast
    self.nodeToScope = nodeToScope
    self.scopeToDecls = scopeToDecls
    self.varToBinding = varToBinding
  }

  /// Creates a scoped program from an AST.
  public init(_ ast: AST) {
    // Establish the scope relationships.
    var s = ScopeVisitor()
    for m in ast.modules {
      ast.walk(m, notifying: &s)
    }

    self.ast = ast
    self.nodeToScope = s.nodeToScope
    self.scopeToDecls = s.scopeToDecls
    self.varToBinding = s.varToBinding
  }

  /// Returns a copy of `self` in which a new module has been loaded, calling `make` to form its
  /// contents and reporting diagnostics to `log`.
  public func loadModule(
    reportingDiagnosticsTo log: inout DiagnosticSet,
    creatingContentsWith make: AST.ModuleLoader
  ) throws -> (ScopedProgram, ModuleDecl.ID) {
    var extended = ast
    let m = try extended.loadModule(reportingDiagnosticsTo: &log, creatingContentsWith: make)

    var s = ScopeVisitor(extending: self)
    extended.walk(m, notifying: &s)
    let p = ScopedProgram(
      ast: extended,
      nodeToScope: s.nodeToScope,
      scopeToDecls: s.scopeToDecls,
      varToBinding: s.varToBinding)

    try log.throwOnError()
    return (p, m)
  }

}

/// The state of the visitor building scope relationships.
private struct ScopeVisitor: ASTWalkObserver {

  /// A map from node to the innermost scope that contains it.
  var nodeToScope: ASTProperty<AnyScopeID>

  /// A map from scope to the declarations that it contains.
  var scopeToDecls: ASTProperty<DeclIDs>

  /// A map from variable declaration its containing binding declaration.
  var varToBinding: [VarDecl.ID: BindingDecl.ID]

  /// A stack containing the bindind declarations currently visited.
  var bindingDecls: [BindingDecl.ID]

  /// The innermost lexical scope currently visited.
  var innermost: AnyScopeID?

  /// Creates a new instance.
  init() {
    self.nodeToScope = [:]
    self.scopeToDecls = [:]
    self.varToBinding = [:]
    self.bindingDecls = []
    self.innermost = nil
  }

  /// Creates an instance storing its result in a copy of `p`'s properties.
  init(extending p: ScopedProgram) {
    self.nodeToScope = p.nodeToScope
    self.scopeToDecls = p.scopeToDecls
    self.varToBinding = p.varToBinding
    self.bindingDecls = []
    self.innermost = nil
  }

  /// Inserts `child` into `scope`.
  private mutating func insert(child: AnyNodeID, into scope: AnyScopeID) {
    assert(child.kind != ModuleDecl.self)
    nodeToScope[child] = scope

    if let d = AnyDeclID(child) {
      scopeToDecls[scope]!.insert(d)
    }
  }

  mutating func willEnter(_ n: AnyNodeID, in ast: AST) -> Bool {
    if let m = ModuleDecl.ID(n) {
      return visit(moduleDecl: .init(m)!, in: ast)
    }

    insert(child: n, into: innermost!)

    switch n.kind {
    case BindingDecl.self:
      return visit(bindingDecl: .init(n)!, in: ast)
    case ConformanceDecl.self:
      return visit(conformanceDecl: .init(n)!, in: ast)
    case ExtensionDecl.self:
      return visit(extensionDecl: .init(n)!, in: ast)
    case ProductTypeDecl.self:
      return visit(productTypeDecl: .init(n)!, in: ast)
    case TraitDecl.self:
      return visit(traitDecl: .init(n)!, in: ast)
    case VarDecl.self:
      return visit(varDecl: .init(n)!, in: ast)
    case ConditionalExpr.self:
      return visit(conditionalExpr: .init(n)!, in: ast)
    case ConditionalStmt.self:
      return visit(conditionalStmt: .init(n)!, in: ast)
    case DoWhileStmt.self:
      return visit(doWhileStmt: .init(n)!, in: ast)
    default:
      break
    }

    if let s = AnyScopeID(n) {
      scopeToDecls[s] = .init()
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
      innermost = nodeToScope[s]!
    }
  }

  private mutating func visit(bindingDecl d: BindingDecl.ID, in ast: AST) -> Bool {
    bindingDecls.append(d)
    ast.traverse(ast[d], notifying: &self)
    bindingDecls.removeLast()
    return false
  }

  private mutating func visit(conformanceDecl d: ConformanceDecl.ID, in ast: AST) -> Bool {
    scopeToDecls[d] = .init()

    // The subject and conformance list reside in the parent's scope.
    innermost = nodeToScope[d]
    ast.walk(ast[d].subject, notifying: &self)
    ast.walk(roots: ast[d].conformances, notifying: &self)

    // Other parts reside in `d`.
    innermost = AnyScopeID(d)
    ast[d].whereClause.map({ ast.traverse(whereClause: $0.value, notifying: &self) })
    ast.walk(roots: ast[d].members, notifying: &self)

    innermost = nodeToScope[d]
    return false
  }

  private mutating func visit(extensionDecl d: ExtensionDecl.ID, in ast: AST) -> Bool {
    scopeToDecls[d] = .init()

    // The subject resides in the parent's scope.
    innermost = nodeToScope[d]
    ast.walk(ast[d].subject, notifying: &self)

    // Other parts reside in `d`.
    innermost = AnyScopeID(d)
    ast[d].whereClause.map({ ast.traverse(whereClause: $0.value, notifying: &self) })
    ast.walk(roots: ast[d].members, notifying: &self)

    innermost = nodeToScope[d]
    return false
  }

  private mutating func visit(moduleDecl d: ModuleDecl.ID, in ast: AST) -> Bool {
    assert(innermost == nil)

    innermost = AnyScopeID(d)
    ast.traverse(ast[d], notifying: &self)
    scopeToDecls[d] = DeclIDs(formingUnionOf: ast[d].sources.map({ scopeToDecls[$0]! }))

    innermost = nil
    return false
  }

  private mutating func visit(productTypeDecl d: ProductTypeDecl.ID, in ast: AST) -> Bool {
    scopeToDecls[d] = .init()

    // The conformance list resides in the parent's scope.
    innermost = nodeToScope[d]
    ast.walk(roots: ast[d].conformances, notifying: &self)

    // Other parts reside in `d`.
    innermost = AnyScopeID(d)
    ast[d].genericClause.map({ ast.traverse(genericClause: $0.value, notifying: &self) })
    ast.walk(roots: ast[d].members, notifying: &self)

    innermost = nodeToScope[d]
    return false
  }

  private mutating func visit(traitDecl d: TraitDecl.ID, in ast: AST) -> Bool {
    scopeToDecls[d] = .init()

    // The bounds reside in the parent's scope.
    innermost = nodeToScope[d]
    ast.walk(roots: ast[d].bounds, notifying: &self)

    // Other parts reside in `d`.
    innermost = AnyScopeID(d)
    ast.walk(roots: ast[d].members, notifying: &self)

    innermost = nodeToScope[d]
    return false
  }

  private mutating func visit(varDecl d: VarDecl.ID, in ast: AST) -> Bool {
    // FIXME: incorrect if we're in a match case
    varToBinding[d] = bindingDecls.last
    return false
  }

  private mutating func visit(conditionalExpr e: ConditionalExpr.ID, in ast: AST) -> Bool {
    scopeToDecls[e] = .init()
    innermost = AnyScopeID(e)
    ast.walk(conditionItems: ast[e].condition, notifying: &self)
    ast.walk(ast[e].success, notifying: &self)

    // The failure branch is not in the scope of the conditional expression.
    innermost = nodeToScope[e]!
    ast.walk(ast[e].failure.value, notifying: &self)
    return false
  }

  private mutating func visit(conditionalStmt s: ConditionalStmt.ID, in ast: AST) -> Bool {
    scopeToDecls[s] = .init()
    innermost = AnyScopeID(s)
    ast.walk(conditionItems: ast[s].condition, notifying: &self)
    ast.walk(ast[s].success, notifying: &self)

    // The failure branch is not in the scope of the conditional expression.
    innermost = nodeToScope[s]!
    ast.walk(ast[s].failure?.value, notifying: &self)
    return false
  }

  private mutating func visit(doWhileStmt s: DoWhileStmt.ID, in ast: AST) -> Bool {
    insert(child: AnyNodeID(ast[s].body), into: innermost!)
    scopeToDecls[ast[s].body] = .init()
    innermost = AnyScopeID(ast[s].body)
    ast.walk(roots: ast[ast[s].body].stmts, notifying: &self)

    // The condition is in the same scope as the body.
    ast.walk(ast[s].condition.value, notifying: &self)
    innermost = nodeToScope[ast[s].body]!
    return false
  }

}
