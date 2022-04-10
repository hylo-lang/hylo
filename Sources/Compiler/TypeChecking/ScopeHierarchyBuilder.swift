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

  /// The index of the current innermost lexical scope.
  private var innermost: AnyNodeIndex?

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
    in scope: NodeIndex<T>,
    _ action: (inout ScopeHierarchyBuilder) -> Void
  ) {
    let i = AnyNodeIndex(scope)
    hierarchy.parent[i] = innermost
    innermost = i
    action(&self)
    innermost = hierarchy.parent[i]
  }

  // MARK: Declarations

  mutating func visit(associatedType decl: NodeIndex<AssociatedTypeDecl>) {
    hierarchy.container[decl] = innermost
    visit(whereClause: ast[decl].whereClause?.value)
  }

  mutating func visit(binding decl: NodeIndex<BindingDecl>) {
    hierarchy.container[decl] = innermost
    visit(binding: ast[decl].pattern)
    ast[decl].initializer?.accept(&self)
  }

  mutating func visit(conformance decl: NodeIndex<ConformanceDecl>) {
    hierarchy.container[decl] = innermost
    nesting(in: decl, { this in
      this.ast[decl].subject.accept(&this)
      this.visit(whereClause: this.ast[decl].whereClause?.value)
      for i in this.ast[decl].members {
        i.accept(&this)
      }
    })
  }

  mutating func visit(extension decl: NodeIndex<ExtensionDecl>) {
    hierarchy.container[decl] = innermost
    nesting(in: decl, { this in
      this.ast[decl].subject.accept(&this)
      this.visit(whereClause: this.ast[decl].whereClause?.value)
      for i in this.ast[decl].members {
        i.accept(&this)
      }
    })
  }

  mutating func visit(fun decl: NodeIndex<FunDecl>) {
    hierarchy.container[decl] = innermost
    nesting(in: decl, { this in
      for i in this.ast[decl].captures {
        this.visit(binding: i)
      }

      for i in this.ast[decl].parameters {
        this.visit(param: i)
      }

      this.ast[decl].output?.accept(&this)

      switch this.ast[decl].body?.value {
      case let .expr(expr):
        expr.accept(&this)
      case let .block(stmt):
        this.visit(brace: stmt)
      case let .bundle(impls):
        for i in impls {
          this.visit(methodImpl: i)
        }
      case nil:
        break
      }
    })
  }

  mutating func visit(genericSizeParam decl: NodeIndex<GenericSizeParamDecl>) {
    hierarchy.container[decl] = innermost
  }

  mutating func visit(genericTypeParam decl: NodeIndex<GenericTypeParamDecl>) {
    hierarchy.container[decl] = innermost
  }

  mutating func visit(methodImpl decl: NodeIndex<MethodImplDecl>) {
    hierarchy.container[decl] = innermost
    switch ast[decl].body?.value {
    case let .expr(expr):
      expr.accept(&self)
    case let .block(stmt):
      visit(brace: stmt)
    case nil:
      break
    }
  }

  mutating func visit(module decl: NodeIndex<ModuleDecl>) {
    innermost = AnyNodeIndex(decl)
    for i in ast[decl].members {
      i.accept(&self)
    }
  }

  mutating func visit(namespace decl: NodeIndex<NamespaceDecl>) {
    hierarchy.container[decl] = innermost
    nesting(in: decl, { this in
      for i in this.ast[decl].members {
        i.accept(&this)
      }
    })
  }

  mutating func visit(param decl: NodeIndex<ParamDecl>) {
    hierarchy.container[decl] = innermost
    ast[decl].annotation?.accept(&self)
    ast[decl].defaultValue?.accept(&self)
  }

  mutating func visit(productType decl: NodeIndex<ProductTypeDecl>) {
    hierarchy.container[decl] = innermost
    nesting(in: decl, { this in
      this.visit(genericClause: this.ast[decl].genericClause?.value)

      for i in this.ast[decl].members {
        i.accept(&this)
      }
    })
  }

  mutating func visit(subscript decl: NodeIndex<SubscriptDecl>) {
    hierarchy.container[decl] = innermost
    nesting(in: decl, { this in
      for i in this.ast[decl].captures {
        this.visit(binding: i)
      }

      for i in this.ast[decl].parameters {
        this.visit(param: i)
      }

      this.ast[decl].output.accept(&this)

      for i in this.ast[decl].impls {
        this.visit(subscriptImpl: i)
      }
    })
  }

  mutating func visit(subscriptImpl decl: NodeIndex<SubscriptImplDecl>) {
    hierarchy.container[decl] = innermost
    ast[decl].body.map({ stmt in visit(brace: stmt) })
  }

  mutating func visit(trait decl: NodeIndex<TraitDecl>) {
    hierarchy.container[decl] = innermost
    nesting(in: decl, { this in
      for i in this.ast[decl].members {
        i.accept(&this)
      }
    })
  }

  mutating func visit(typeAlias decl: NodeIndex<TypeAliasDecl>) {
    hierarchy.container[decl] = innermost
    nesting(in: decl, { this in
      this.visit(genericClause: this.ast[decl].genericClause?.value)

      switch this.ast[decl].body.value {
      case let .union(members):
        for i in members {
          this.visit(productType: i)
        }

      case let .typeExpr(type):
        type.accept(&this)
      }
    })
  }

  mutating func visit(`var` decl: NodeIndex<VarDecl>) {
    hierarchy.container[decl] = innermost
  }

  // MARK: Expressions

  mutating func visit(async expr: NodeIndex<AsyncExpr>) {
    visit(fun: ast[expr].decl)
  }

  mutating func visit(await expr: NodeIndex<AwaitExpr>) {
    ast[expr].operand.accept(&self)
  }

  mutating func visit(boolLiteral expr: NodeIndex<BoolLiteralExpr>) {}

  mutating func visit(bufferLiteral expr: NodeIndex<BufferLiteralExpr>) {
    for e in ast[expr].elements {
      e.accept(&self)
    }
  }

  mutating func visit(charLiteral expr: NodeIndex<CharLiteralExpr>) {}

  mutating func visit(cond expr: NodeIndex<CondExpr>) {
    nesting(in: expr, { this in
      for item in this.ast[expr].condition {
        switch item.value {
        case let .expr(expr):
          expr.accept(&this)
        case let .decl(i):
          this.visit(binding: i)
        }
      }

      switch this.ast[expr].success {
      case let .expr(expr):
        expr.accept(&this)
      case let .block(stmt):
        this.visit(brace: stmt)
      }

      switch this.ast[expr].failure {
      case let .expr(expr):
        expr.accept(&this)
      case let .block(stmt):
        this.visit(brace: stmt)
      case nil:
        break
      }
    })
  }

  mutating func visit(floatLiteral expr: NodeIndex<FloatLiteralExpr>) {}

  mutating func visit(funCall expr: NodeIndex<FunCallExpr>) {
    ast[expr].callee.accept(&self)
    for a in ast[expr].arguments {
      a.value.value.accept(&self)
    }
  }

  mutating func visit(intLiteral expr: NodeIndex<IntLiteralExpr>) {}

  mutating func visit(lambda expr: NodeIndex<LambdaExpr>) {
    visit(fun: ast[expr].decl)
  }

  mutating func visit(mapLiteral expr: NodeIndex<MapLiteralExpr>) {
    for e in ast[expr].elements {
      e.value.key.accept(&self)
      e.value.value.accept(&self)
    }
  }

  mutating func visit(match expr: NodeIndex<MatchExpr>) {
    ast[expr].subject.accept(&self)

    for e in ast[expr].cases {
      visit(matchCase: e)
    }
  }

  mutating func visit(matchCase expr: NodeIndex<MatchCaseExpr>) {
    nesting(in: expr, { this in
      this.ast[expr].pattern.accept(&this)
      this.ast[expr].condition?.accept(&this)
      switch this.ast[expr].body.value {
      case let .expr(expr):
        expr.accept(&this)
      case let .block(stmt):
        this.visit(brace: stmt)
      }
    })
  }

  mutating func visit(name expr: NodeIndex<NameExpr>) {
    if case let .explicit(domain) = ast[expr].domain {
      domain.accept(&self)
    }

    for a in ast[expr].arguments {
      switch a.value {
      case let .type(a):
        a.accept(&self)
      case let .size(a):
        a.accept(&self)
      }
    }
  }

  mutating func visit(nil expr: NodeIndex<NilExpr>) {}

  mutating func visit(storedProjection expr: NodeIndex<StoredProjectionExpr>) {
    ast[expr].operand.accept(&self)
  }

  mutating func visit(stringLiteral expr: NodeIndex<StringLiteralExpr>) {}

  mutating func visit(subscriptCall expr: NodeIndex<SubscriptCallExpr>) {
    ast[expr].callee.accept(&self)
    for a in ast[expr].arguments {
      a.value.value.accept(&self)
    }
  }

  mutating func visit(tuple expr: NodeIndex<TupleExpr>) {
    for e in ast[expr].elements {
      e.value.value.accept(&self)
    }
  }

  mutating func visit(unfolded expr: NodeIndex<UnfoldedExpr>) {
    for e in ast[expr].subexpressions {
      e.accept(&self)
    }
  }

  // MARK: Patterns

  mutating func visit(binding pattern: NodeIndex<BindingPattern>) {
    ast[pattern].subpattern.accept(&self)
    ast[pattern].annotation?.accept(&self)
  }

  mutating func visit(expr pattern: NodeIndex<ExprPattern>) {
    ast[pattern].expr.accept(&self)
  }

  mutating func visit(name pattern: NodeIndex<NamePattern>) {}

  mutating func visit(tuple pattern: NodeIndex<TuplePattern>) {
    for e in ast[pattern].elements {
      e.value.pattern.accept(&self)
    }
  }

  mutating func visit(wildcard pattern: NodeIndex<WildcardPattern>) {}

  // MARK: Statements

  mutating func visit(brace stmt: NodeIndex<BraceStmt>) {
    nesting(in: stmt, { this in
      for s in this.ast[stmt].stmts {
        s.accept(&this)
      }
    })
  }

  mutating func visit(break stmt: NodeIndex<BreakStmt>) {}

  mutating func visit(continue stmt: NodeIndex<ContinueStmt>) {}

  mutating func visit(decl stmt: NodeIndex<DeclStmt>) {
    ast[stmt].decl.accept(&self)
  }

  mutating func visit(doWhile stmt: NodeIndex<DoWhileStmt>) {
    visit(brace: ast[stmt].body)

    // Visit the condition of the loop in the same lexical scope as the body.
    innermost = AnyNodeIndex(ast[stmt].body)
    ast[stmt].condition.accept(&self)
    innermost = hierarchy.parent[ast[stmt].body]
  }

  mutating func visit(expr stmt: NodeIndex<ExprStmt>) {
    ast[stmt].expr.accept(&self)
  }

  mutating func visit(for stmt: NodeIndex<ForStmt>) {
    nesting(in: stmt, { this in
      this.visit(binding: this.ast[stmt].binding)
      this.ast[stmt].filter?.accept(&this)
      this.visit(brace: this.ast[stmt].body)
    })
  }

  mutating func visit(return stmt: NodeIndex<ReturnStmt>) {
    ast[stmt].value?.accept(&self)
  }

  mutating func visit(while stmt: NodeIndex<WhileStmt>) {
    nesting(in: stmt, { this in
      for item in this.ast[stmt].condition {
        switch item.value {
        case let .expr(expr):
          expr.accept(&this)
        case let .decl(i):
          this.visit(binding: i)
        }
      }

      this.visit(brace: this.ast[stmt].body)
    })
  }

  mutating func visit(yield stmt: NodeIndex<YieldStmt>) {
    ast[stmt].value.accept(&self)
  }

  // MARK: Type expressions

  mutating func visit(async type: NodeIndex<AsyncTypeExpr>) {
    ast[type].operand.accept(&self)
  }

  mutating func visit(conformanceLens type: NodeIndex<ConformanceLensTypeExpr>) {
    ast[type].base.accept(&self)
  }

  mutating func visit(existential type: NodeIndex<ExistentialTypeExpr>) {
    visit(whereClause: ast[type].whereClause?.value)
  }

  mutating func visit(indirect type: NodeIndex<IndirectTypeExpr>) {
    ast[type].operand.accept(&self)
  }

  mutating func visit(lambda type: NodeIndex<LambdaTypeExpr>) {
    ast[type].environment?.accept(&self)
    for p in ast[type].parameters {
      visit(param: p)
    }
    ast[type].output.accept(&self)
  }

  mutating func visit(name type: NodeIndex<NameTypeExpr>) {
    ast[type].domain?.accept(&self)

    for a in ast[type].arguments {
      switch a.value {
      case let .type(a):
        a.accept(&self)
      case let .size(a):
        a.accept(&self)
      }
    }
  }

  mutating func visit(param type: NodeIndex<ParamTypeExpr>) {
    ast[type].bareType.accept(&self)
  }

  mutating func visit(storedProjection type: NodeIndex<StoredProjectionTypeExpr>) {
    ast[type].operand.accept(&self)
  }

  mutating func visit(tuple type: NodeIndex<TupleTypeExpr>) {
    for e in ast[type].elements {
      e.value.type.accept(&self)
    }
  }

  mutating func visit(union type: NodeIndex<UnionTypeExpr>) {
    for e in ast[type].elements {
      e.accept(&self)
    }
  }

  // MARK: Other nodes

  mutating func visit(genericClause clause: GenericClause?) {
    visit(whereClause: clause?.whereClause?.value)
  }

  mutating func visit(whereClause clause: WhereClause?) {
    guard let clause = clause else { return }

    for constraint in clause.constraints {
      switch constraint {
      case let .equality(lhs, rhs):
        lhs.accept(&self)
        rhs.accept(&self)

      case let .conformance(lhs, _):
        visit(name: lhs)

      case let .size(expr):
        expr.accept(&self)
      }
    }
  }

}
