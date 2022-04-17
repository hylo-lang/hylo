import Utils

/// An AST visitor that creates the scope hierarchy of an AST.
struct ScopeHierarchyBuilder:
  DeclVisitor, ExprVisitor, PatternVisitor, StmtVisitor, TypeExprVisitor
{

  typealias Result = Void

  /// The AST of the module for which the scope hierarchy is built.
  private var ast: AST!

  /// The scope hierarchy under construction.
  private var hierarchy: ScopeHierarchy!

  /// The ID of the current innermost lexical scope.
  private var innermost: AnyNodeID?

  /// Returns the scope hierarchy of `ast`.
  mutating func build(hierarchyOf ast: AST) -> ScopeHierarchy {
    self.ast = ast
    hierarchy = ScopeHierarchy()
    for module in ast.modules {
      visit(module: module)
    }
    return hierarchy.release()
  }

  private mutating func nesting<T: Node & LexicalScope>(
    in scope: NodeID<T>,
    _ action: (inout ScopeHierarchyBuilder) -> Void
  ) {
    let i = AnyNodeID(scope)
    hierarchy.parent[i] = innermost
    innermost = i
    action(&self)
    innermost = hierarchy.parent[i]
  }

  // MARK: Declarations

  mutating func visit(associatedSize i: NodeID<AssociatedSizeDecl>) {
    hierarchy.insert(decl: i, into: innermost!)
    visit(whereClause: ast[i].whereClause?.value)
  }

  mutating func visit(associatedType i: NodeID<AssociatedTypeDecl>) {
    hierarchy.insert(decl: i, into: innermost!)
    for j in ast[i].conformances {
      visit(name: j)
    }
    visit(whereClause: ast[i].whereClause?.value)
  }

  mutating func visit(binding i: NodeID<BindingDecl>) {
    hierarchy.insert(decl: i, into: innermost!)
    ast[i].pattern.accept(&self)
    ast[i].initializer?.accept(&self)
  }

  mutating func visit(conformance i: NodeID<ConformanceDecl>) {
    hierarchy.insert(decl: i, into: innermost!)
    nesting(in: i, { this in
      let decl = this.ast[i]
      decl.subject.accept(&this)
      this.visit(whereClause: decl.whereClause?.value)
      for member in decl.members {
        member.accept(&this)
      }
    })
  }

  mutating func visit(extension i: NodeID<ExtensionDecl>) {
    hierarchy.insert(decl: i, into: innermost!)
    nesting(in: i, { this in
      let decl = this.ast[i]
      decl.subject.accept(&this)
      this.visit(whereClause: decl.whereClause?.value)
      for member in decl.members {
        member.accept(&this)
      }
    })
  }

  mutating func visit(fun i: NodeID<FunDecl>) {
    hierarchy.insert(decl: i, into: innermost!)
    nesting(in: i, { this in
      let decl = this.ast[i]
      for capture in decl.captures {
        this.visit(binding: capture)
      }

      for param in decl.parameters {
        this.visit(param: param)
      }

      decl.output?.accept(&this)

      switch decl.body?.value {
      case let .expr(expr):
        expr.accept(&this)
      case let .block(stmt):
        this.visit(brace: stmt)
      case let .bundle(impls):
        for impl in impls {
          this.visit(methodImpl: impl)
        }
      case nil:
        break
      }
    })
  }

  mutating func visit(genericSizeParam i: NodeID<GenericSizeParamDecl>) {
    hierarchy.insert(decl: i, into: innermost!)
  }

  mutating func visit(genericTypeParam i: NodeID<GenericTypeParamDecl>) {
    hierarchy.insert(decl: i, into: innermost!)
  }

  mutating func visit(methodImpl i: NodeID<MethodImplDecl>) {
    hierarchy.insert(decl: i, into: innermost!)
    switch ast[i].body?.value {
    case let .expr(expr):
      expr.accept(&self)
    case let .block(stmt):
      visit(brace: stmt)
    case nil:
      break
    }
  }

  mutating func visit(module i: NodeID<ModuleDecl>) {
    innermost = AnyNodeID(i)
    for member in ast[i].members {
      member.accept(&self)
    }
  }

  mutating func visit(namespace i: NodeID<NamespaceDecl>) {
    hierarchy.insert(decl: i, into: innermost!)
    nesting(in: i, { this in
      for member in this.ast[i].members {
        member.accept(&this)
      }
    })
  }

  mutating func visit(param i: NodeID<ParamDecl>) {
    hierarchy.insert(decl: i, into: innermost!)
    ast[i].annotation?.accept(&self)
    ast[i].defaultValue?.accept(&self)
  }

  mutating func visit(productType i: NodeID<ProductTypeDecl>) {
    hierarchy.insert(decl: i, into: innermost!)
    nesting(in: i, { this in
      this.visit(genericClause: this.ast[i].genericClause?.value)
      for member in this.ast[i].members {
        member.accept(&this)
      }
    })
  }

  mutating func visit(subscript i: NodeID<SubscriptDecl>) {
    hierarchy.insert(decl: i, into: innermost!)
    nesting(in: i, { this in
      let decl = this.ast[i]
      for capture in decl.captures {
        this.visit(binding: capture)
      }

      if let params = decl.parameters {
        for param in params {
          this.visit(param: param)
        }
      }

      decl.output.accept(&this)

      for impl in decl.impls {
        this.visit(subscriptImpl: impl)
      }
    })
  }

  mutating func visit(subscriptImpl i: NodeID<SubscriptImplDecl>) {
    hierarchy.insert(decl: i, into: innermost!)
    ast[i].body.map({ stmt in visit(brace: stmt) })
  }

  mutating func visit(trait i: NodeID<TraitDecl>) {
    hierarchy.insert(decl: i, into: innermost!)
    nesting(in: i, { this in
      for member in this.ast[i].members {
        member.accept(&this)
      }
    })
  }

  mutating func visit(typeAlias i: NodeID<TypeAliasDecl>) {
    hierarchy.insert(decl: i, into: innermost!)
    nesting(in: i, { this in
      let decl = this.ast[i]
      this.visit(genericClause: decl.genericClause?.value)

      switch decl.body.value {
      case let .union(members):
        for member in members {
          this.visit(productType: member)
        }

      case let .typeExpr(type):
        type.accept(&this)
      }
    })
  }

  mutating func visit(`var` i: NodeID<VarDecl>) {
    hierarchy.insert(decl: i, into: innermost!)
  }

  // MARK: Expressions

  mutating func visit(async i: NodeID<AsyncExpr>) {
    visit(fun: ast[i].decl)
  }

  mutating func visit(await i: NodeID<AwaitExpr>) {
    ast[i].operand.accept(&self)
  }

  mutating func visit(boolLiteral i: NodeID<BoolLiteralExpr>) {}

  mutating func visit(bufferLiteral i: NodeID<BufferLiteralExpr>) {
    for elem in ast[i].elements {
      elem.accept(&self)
    }
  }

  mutating func visit(charLiteral i: NodeID<CharLiteralExpr>) {}

  mutating func visit(cond i: NodeID<CondExpr>) {
    nesting(in: i, { this in
      let expr = this.ast[i]
      for item in expr.condition {
        switch item.value {
        case let .expr(expr):
          expr.accept(&this)
        case let .decl(decl):
          this.visit(binding: decl)
        }
      }

      switch expr.success {
      case let .expr(expr):
        expr.accept(&this)
      case let .block(stmt):
        this.visit(brace: stmt)
      }

      switch expr.failure {
      case let .expr(expr):
        expr.accept(&this)
      case let .block(stmt):
        this.visit(brace: stmt)
      case nil:
        break
      }
    })
  }

  mutating func visit(floatLiteral i: NodeID<FloatLiteralExpr>) {}

  mutating func visit(funCall i: NodeID<FunCallExpr>) {
    ast[i].callee.accept(&self)
    for arg in ast[i].arguments {
      arg.value.value.accept(&self)
    }
  }

  mutating func visit(intLiteral i: NodeID<IntLiteralExpr>) {}

  mutating func visit(lambda i: NodeID<LambdaExpr>) {
    visit(fun: ast[i].decl)
  }

  mutating func visit(mapLiteral i: NodeID<MapLiteralExpr>) {
    for elem in ast[i].elements {
      elem.value.key.accept(&self)
      elem.value.value.accept(&self)
    }
  }

  mutating func visit(match i: NodeID<MatchExpr>) {
    ast[i].subject.accept(&self)

    for elem in ast[i].cases {
      visit(matchCase: elem)
    }
  }

  mutating func visit(matchCase i: NodeID<MatchCaseExpr>) {
    nesting(in: i, { this in
      let expr = this.ast[i]
      expr.pattern.accept(&this)
      expr.condition?.accept(&this)

      switch expr.body.value {
      case let .expr(expr):
        expr.accept(&this)
      case let .block(stmt):
        this.visit(brace: stmt)
      }
    })
  }

  mutating func visit(name i: NodeID<NameExpr>) {
    if case let .explicit(domain) = ast[i].domain {
      domain.accept(&self)
    }

    for arg in ast[i].arguments {
      switch arg.value {
      case let .size(arg):
        arg.accept(&self)
      case let .type(arg):
        arg.accept(&self)
      }
    }
  }

  mutating func visit(nil i: NodeID<NilExpr>) {}

  mutating func visit(storedProjection i: NodeID<StoredProjectionExpr>) {
    ast[i].operand.accept(&self)
  }

  mutating func visit(stringLiteral i: NodeID<StringLiteralExpr>) {}

  mutating func visit(subscriptCall i: NodeID<SubscriptCallExpr>) {
    ast[i].callee.accept(&self)
    for arg in ast[i].arguments {
      arg.value.value.accept(&self)
    }
  }

  mutating func visit(tuple i: NodeID<TupleExpr>) {
    for elem in ast[i].elements {
      elem.value.value.accept(&self)
    }
  }

  mutating func visit(unfolded i: NodeID<UnfoldedExpr>) {
    for expr in ast[i].subexpressions {
      expr.accept(&self)
    }
  }

  // MARK: Patterns

  mutating func visit(binding i: NodeID<BindingPattern>) {
    ast[i].subpattern.accept(&self)
    ast[i].annotation?.accept(&self)
  }

  mutating func visit(expr i: NodeID<ExprPattern>) {
    ast[i].expr.accept(&self)
  }

  mutating func visit(name i: NodeID<NamePattern>) {}

  mutating func visit(tuple i: NodeID<TuplePattern>) {
    for elem in ast[i].elements {
      elem.value.pattern.accept(&self)
    }
  }

  mutating func visit(wildcard i: NodeID<WildcardPattern>) {}

  // MARK: Statements

  mutating func visit(brace i: NodeID<BraceStmt>) {
    nesting(in: i, { this in
      for stmt in this.ast[i].stmts {
        stmt.accept(&this)
      }
    })
  }

  mutating func visit(break i: NodeID<BreakStmt>) {}

  mutating func visit(continue i: NodeID<ContinueStmt>) {}

  mutating func visit(decl i: NodeID<DeclStmt>) {
    ast[i].decl.accept(&self)
  }

  mutating func visit(doWhile i: NodeID<DoWhileStmt>) {
    visit(brace: ast[i].body)

    // Visit the condition of the loop in the same lexical scope as the body.
    innermost = AnyNodeID(ast[i].body)
    ast[i].condition.accept(&self)
    innermost = hierarchy.parent[ast[i].body]
  }

  mutating func visit(expr i: NodeID<ExprStmt>) {
    ast[i].expr.accept(&self)
  }

  mutating func visit(for i: NodeID<ForStmt>) {
    nesting(in: i, { this in
      this.visit(binding: this.ast[i].binding)
      this.ast[i].filter?.accept(&this)
      this.visit(brace: this.ast[i].body)
    })
  }

  mutating func visit(return i: NodeID<ReturnStmt>) {
    ast[i].value?.accept(&self)
  }

  mutating func visit(while i: NodeID<WhileStmt>) {
    nesting(in: i, { this in
      for item in this.ast[i].condition {
        switch item.value {
        case let .expr(expr):
          expr.accept(&this)
        case let .decl(decl):
          this.visit(binding: decl)
        }
      }

      this.visit(brace: this.ast[i].body)
    })
  }

  mutating func visit(yield i: NodeID<YieldStmt>) {
    ast[i].value.accept(&self)
  }

  // MARK: Type expressions

  mutating func visit(async i: NodeID<AsyncTypeExpr>) {
    ast[i].operand.accept(&self)
  }

  mutating func visit(conformanceLens i: NodeID<ConformanceLensTypeExpr>) {
    ast[i].wrapped.accept(&self)
  }

  mutating func visit(existential i: NodeID<ExistentialTypeExpr>) {
    visit(whereClause: ast[i].whereClause?.value)
  }

  mutating func visit(indirect i: NodeID<IndirectTypeExpr>) {
    ast[i].operand.accept(&self)
  }

  mutating func visit(lambda i: NodeID<LambdaTypeExpr>) {
    ast[i].environment?.accept(&self)
    for param in ast[i].parameters {
      visit(param: param)
    }
    ast[i].output.accept(&self)
  }

  mutating func visit(name i: NodeID<NameTypeExpr>) {
    ast[i].domain?.accept(&self)

    for arg in ast[i].arguments {
      switch arg {
      case let .size(arg):
        arg.accept(&self)
      case let .type(arg):
        arg.accept(&self)
      }
    }
  }

  mutating func visit(param i: NodeID<ParamTypeExpr>) {
    ast[i].bareType.accept(&self)
  }

  mutating func visit(storedProjection i: NodeID<StoredProjectionTypeExpr>) {
    ast[i].operand.accept(&self)
  }

  mutating func visit(tuple i: NodeID<TupleTypeExpr>) {
    for elem in ast[i].elements {
      elem.value.type.accept(&self)
    }
  }

  mutating func visit(union i: NodeID<UnionTypeExpr>) {
    for elem in ast[i].elements {
      elem.accept(&self)
    }
  }

  // MARK: Other nodes

  mutating func visit(genericClause clause: GenericClause?) {
    guard let clause = clause else { return }

    for i in clause.params {
      switch i {
      case .size(let i):
        visit(genericSizeParam: i)
      case .type(let i):
        visit(genericTypeParam: i)
      }
    }

    visit(whereClause: clause.whereClause?.value)
  }

  mutating func visit(whereClause clause: WhereClause?) {
    guard let clause = clause else { return }

    for constraint in clause.constraints {
      switch constraint.value {
      case let .equality(lhs, rhs):
        lhs.accept(&self)
        rhs.accept(&self)

      case let .conformance(lhs, rhs):
        visit(name: lhs)
        for i in rhs {
          visit(name: i)
        }

      case let .size(expr):
        expr.accept(&self)
      }
    }
  }

}
