import Utils

/// An AST visitor that creates the scope hierarchy of a module.
struct ScopeHierarchyBuilder:
  DeclVisitor, ExprVisitor, PatternVisitor, StmtVisitor, TypeExprVisitor
{

  typealias Result = Void

  /// The AST of the module for which the scope hierarchy is built.
  private var ast: AST!

  /// The scope hierarchy under construction.
  private var hierarchy: ScopeHierarchy!

  /// The index of the current innermost lexical scope.
  private var innermost: ScopeID?

  /// Returns the scope hierarchy of `module`.
  ///
  /// - Requires: `module` is a valid index in `ast`.
  mutating func build(hierarchyOf module: DeclIndex<ModuleDecl>, in ast: AST) -> ScopeHierarchy {
    self.ast = ast
    hierarchy = ScopeHierarchy()
    innermost = ast[module].scopeID
    for i in ast[module].members {
      i.accept(&self)
    }
    return hierarchy.release()
  }

  // MARK: Declarations

  mutating func visit(associatedType decl: DeclIndex<AssociatedTypeDecl>) {
    visit(whereClause: ast[decl].whereClause?.node)
  }

  mutating func visit(binding decl: DeclIndex<BindingDecl>) {
    visit(binding: ast[decl].pattern.node)
    ast[decl].initializer?.node.accept(&self)
  }

  mutating func visit(conformance decl: DeclIndex<ConformanceDecl>) {
    hierarchy.parent[ast[decl].scopeID] = innermost
    innermost = ast[decl].scopeID
    defer { innermost = hierarchy.parent[ast[decl].scopeID] }

    ast[decl].subject.node.accept(&self)
    visit(whereClause: ast[decl].whereClause?.node)
    for i in ast[decl].members {
      i.accept(&self)
    }
  }

  mutating func visit(extension decl: DeclIndex<ExtensionDecl>) {
    hierarchy.parent[ast[decl].scopeID] = innermost
    innermost = ast[decl].scopeID
    defer { innermost = hierarchy.parent[ast[decl].scopeID] }

    ast[decl].subject.node.accept(&self)
    visit(whereClause: ast[decl].whereClause?.node)
    for i in ast[decl].members {
      i.accept(&self)
    }
  }

  mutating func visit(fun decl: DeclIndex<FunDecl>) {
    hierarchy.parent[ast[decl].scopeID] = innermost
    innermost = ast[decl].scopeID
    defer { innermost = hierarchy.parent[ast[decl].scopeID] }

    for i in ast[decl].captures {
      visit(binding: i)
    }

    for i in ast[decl].parameters {
      visit(param: i)
    }

    ast[decl].output?.node.accept(&self)

    switch ast[decl].body?.node {
    case let .expr(expr):
      expr.accept(&self)
    case let .block(stmt):
      visit(brace: stmt)
    case let .bundle(impls):
      for i in impls {
        visit(methodImpl: i)
      }
    case nil:
      break
    }
  }

  mutating func visit(genericSizeParam decl: DeclIndex<GenericSizeParamDecl>) {}

  mutating func visit(genericTypeParam decl: DeclIndex<GenericTypeParamDecl>) {}

  mutating func visit(methodImpl decl: DeclIndex<MethodImplDecl>) {
    switch ast[decl].body?.node {
    case let .expr(expr):
      expr.accept(&self)
    case let .block(stmt):
      visit(brace: stmt)
    case nil:
      break
    }
  }

  mutating func visit(module decl: DeclIndex<ModuleDecl>) { unreachable() }

  mutating func visit(namespace decl: DeclIndex<NamespaceDecl>) {
    hierarchy.parent[ast[decl].scopeID] = innermost
    innermost = ast[decl].scopeID
    defer { innermost = hierarchy.parent[ast[decl].scopeID] }

    for i in ast[decl].members {
      i.accept(&self)
    }
  }

  mutating func visit(param decl: DeclIndex<ParamDecl>) {
    ast[decl].annotation?.node.accept(&self)
    ast[decl].defaultValue?.node.accept(&self)
  }

  mutating func visit(productType decl: DeclIndex<ProductTypeDecl>) {
    hierarchy.parent[ast[decl].scopeID] = innermost
    innermost = ast[decl].scopeID
    defer { innermost = hierarchy.parent[ast[decl].scopeID] }

    visit(genericClause: ast[decl].genericClause?.node)

    for i in ast[decl].members {
      i.accept(&self)
    }
  }

  mutating func visit(subscript decl: DeclIndex<SubscriptDecl>) {
    hierarchy.parent[ast[decl].scopeID] = innermost
    innermost = ast[decl].scopeID
    defer { innermost = hierarchy.parent[ast[decl].scopeID] }

    for i in ast[decl].captures {
      visit(binding: i)
    }

    for i in ast[decl].parameters {
      visit(param: i)
    }

    ast[decl].output.node.accept(&self)

    for i in ast[decl].impls {
      visit(subscriptImpl: i)
    }
  }

  mutating func visit(subscriptImpl decl: DeclIndex<SubscriptImplDecl>) {
    ast[decl].body.map({ stmt in visit(brace: stmt.node) })
  }

  mutating func visit(trait decl: DeclIndex<TraitDecl>) {
    hierarchy.parent[ast[decl].scopeID] = innermost
    innermost = ast[decl].scopeID
    defer { innermost = hierarchy.parent[ast[decl].scopeID] }

    for i in ast[decl].members {
      i.accept(&self)
    }
  }

  mutating func visit(typeAlias decl: DeclIndex<TypeAliasDecl>) {
    hierarchy.parent[ast[decl].scopeID] = innermost
    innermost = ast[decl].scopeID
    defer { innermost = hierarchy.parent[ast[decl].scopeID] }

    visit(genericClause: ast[decl].genericClause?.node)

    switch ast[decl].body.node {
    case let .union(members):
      for i in members {
        visit(productType: i)
      }

    case let .typeExpr(type):
      type.accept(&self)
    }
  }

  mutating func visit(`var` decl: DeclIndex<VarDecl>) {}

  // MARK: Expressions

  mutating func visit(async expr: AsyncExpr) {
    visit(fun: expr.decl)
  }

  mutating func visit(await expr: AwaitExpr) {
    expr.operand.node.accept(&self)
  }

  mutating func visit(boolLiteral expr: BoolLiteralExpr) {}

  mutating func visit(bufferLiteral expr: BufferLiteralExpr) {
    for e in expr.elements {
      e.node.accept(&self)
    }
  }

  mutating func visit(charLiteral expr: CharLiteralExpr) {}

  mutating func visit(cond expr: CondExpr) {
    hierarchy.parent[expr.scopeID] = innermost
    innermost = expr.scopeID
    defer { innermost = hierarchy.parent[expr.scopeID] }

    for item in expr.condition {
      switch item.node {
      case let .expr(expr):
        expr.accept(&self)
      case let .decl(i):
        visit(binding: i)
      }
    }

    switch expr.success.node {
    case let .expr(expr):
      expr.accept(&self)
    case let .block(stmt):
      visit(brace: stmt)
    }

    switch expr.failure?.node {
    case let .expr(expr):
      expr.accept(&self)
    case let .block(stmt):
      visit(brace: stmt)
    case nil:
      break
    }
  }

  mutating func visit(floatLiteral expr: FloatLiteralExpr) {}

  mutating func visit(funCall expr: FunCallExpr) {
    expr.callee.node.accept(&self)
    for a in expr.arguments {
      a.node.value.node.accept(&self)
    }
  }

  mutating func visit(intLiteral expr: IntLiteralExpr) {}

  mutating func visit(lambda expr: LambdaExpr) {
    visit(fun: expr.decl)
  }

  mutating func visit(mapLiteral expr: MapLiteralExpr) {
    for e in expr.elements {
      e.node.key.node.accept(&self)
      e.node.value.node.accept(&self)
    }
  }

  mutating func visit(match expr: MatchExpr) {
    expr.subject.node.accept(&self)

    for c in expr.cases {
      hierarchy.parent[c.node.scopeID] = innermost
      innermost = c.node.scopeID
      defer { innermost = hierarchy.parent[c.node.scopeID] }

      c.node.pattern.node.accept(&self)
      c.node.condition?.node.accept(&self)
      switch c.node.body.node {
      case let .expr(expr):
        expr.accept(&self)
      case let .block(stmt):
        visit(brace: stmt)
      }
    }
  }

  mutating func visit(name expr: NameExpr) {
    if case let .explicit(domain) = expr.domain {
      domain.node.accept(&self)
    }

    for a in expr.arguments {
      switch a.node {
      case let .type(a):
        a.accept(&self)
      case let .size(a):
        a.accept(&self)
      }
    }
  }

  mutating func visit(nil expr: NilExpr) {}

  mutating func visit(storedProjection expr: StoredProjectionExpr) {
    expr.operand.node.accept(&self)
  }

  mutating func visit(stringLiteral expr: StringLiteralExpr) {}

  mutating func visit(subscriptCall expr: SubscriptCallExpr) {
    expr.callee.node.accept(&self)
    for a in expr.arguments {
      a.node.value.node.accept(&self)
    }
  }

  mutating func visit(tuple expr: TupleExpr) {
    for e in expr.elements {
      e.node.value.node.accept(&self)
    }
  }

  mutating func visit(unfolded expr: UnfoldedExpr) {
    for e in expr.subexpressions {
      e.node.accept(&self)
    }
  }

  // MARK: Patterns

  mutating func visit(binding pattern: BindingPattern) {
    pattern.subpattern.node.accept(&self)
    pattern.annotation?.node.accept(&self)
  }

  mutating func visit(expr pattern: ExprPattern) {
    pattern.expr.node.accept(&self)
  }

  mutating func visit(name pattern: NamePattern) {}

  mutating func visit(tuple pattern: TuplePattern) {
    for e in pattern.elements {
      e.node.pattern.node.accept(&self)
    }
  }

  mutating func visit(wildcard pattern: WildcardPattern) {}

  // MARK: Statements

  mutating func visit(brace stmt: BraceStmt) {
    hierarchy.parent[stmt.scopeID] = innermost
    innermost = stmt.scopeID
    defer { innermost = hierarchy.parent[stmt.scopeID] }

    for s in stmt.stmts {
      s.node.accept(&self)
    }
  }

  mutating func visit(break stmt: BreakStmt) {}

  mutating func visit(continue stmt: ContinueStmt) {}

  mutating func visit(decl stmt: DeclStmt) {
    stmt.decl.accept(&self)
  }

  mutating func visit(doWhile stmt: DoWhileStmt) {
    visit(brace: stmt.body.node)

    // Visit the condition of the loop in the same lexical scope as the body.
    innermost = stmt.body.node.scopeID
    stmt.condition.node.accept(&self)
  }

  mutating func visit(expr stmt: ExprStmt) {
    stmt.expr.accept(&self)
  }

  mutating func visit(for stmt: ForStmt) {
    hierarchy.parent[stmt.scopeID] = innermost
    innermost = stmt.scopeID
    defer { innermost = hierarchy.parent[stmt.scopeID] }

    visit(binding: stmt.binding)
    stmt.filter?.node.accept(&self)
    visit(brace: stmt.body.node)
  }

  mutating func visit(return stmt: ReturnStmt) {
    stmt.value?.node.accept(&self)
  }

  mutating func visit(while stmt: WhileStmt) {
    hierarchy.parent[stmt.scopeID] = innermost
    innermost = stmt.scopeID
    defer { innermost = hierarchy.parent[stmt.scopeID] }

    for item in stmt.condition {
      switch item.node {
      case let .expr(expr):
        expr.accept(&self)
      case let .decl(i):
        visit(binding: i)
      }
    }

    visit(brace: stmt.body.node)
  }

  mutating func visit(yield stmt: YieldStmt) {
    stmt.value.node.accept(&self)
  }

  // MARK: Type expressions

  mutating func visit(async type: AsyncTypeExpr) {
    type.operand.node.accept(&self)
  }

  mutating func visit(conformanceLens type: ConformanceLensTypeExpr) {
    type.base.node.accept(&self)
  }

  mutating func visit(existential type: ExistentialTypeExpr) {
    visit(whereClause: type.whereClause?.node)
  }

  mutating func visit(indirect type: IndirectTypeExpr) {
    type.operand.node.accept(&self)
  }

  mutating func visit(lambda type: LambdaTypeExpr) {
    type.environment?.node.accept(&self)
    for p in type.parameters {
      visit(param: p.node)
    }
    type.output.node.accept(&self)
  }

  mutating func visit(name type: NameTypeExpr) {
    type.domain?.node.accept(&self)

    for a in type.arguments {
      switch a.node {
      case let .type(a):
        a.accept(&self)
      case let .size(a):
        a.accept(&self)
      }
    }
  }

  mutating func visit(param type: ParamTypeExpr) {
    type.bareType.node.accept(&self)
  }

  mutating func visit(storedProjection type: StoredProjectionTypeExpr) {
    type.operand.node.accept(&self)
  }

  mutating func visit(tuple type: TupleTypeExpr) {
    for e in type.elements {
      e.node.type.node.accept(&self)
    }
  }

  mutating func visit(union type: UnionTypeExpr) {
    for e in type.elements {
      e.node.accept(&self)
    }
  }

  // MARK: Other nodes

  mutating func visit(genericClause clause: GenericClause?) {
    visit(whereClause: clause?.whereClause?.node)
  }

  mutating func visit(whereClause clause: WhereClause?) {
    guard let clause = clause else { return }

    for constraint in clause.constraints {
      switch constraint {
      case let .equality(lhs, rhs):
        lhs.node.accept(&self)
        rhs.node.accept(&self)

      case let .conformance(lhs, _):
        visit(name: lhs.node)

      case let .size(expr):
        expr.node.accept(&self)
      }
    }
  }

}
