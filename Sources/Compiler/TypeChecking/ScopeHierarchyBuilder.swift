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

  mutating func visit(associatedType i: NodeIndex<AssociatedTypeDecl>) {
    hierarchy.insert(decl: i, into: innermost!)
    visit(whereClause: ast[i].whereClause?.value)
  }

  mutating func visit(binding i: NodeIndex<BindingDecl>) {
    hierarchy.insert(decl: i, into: innermost!)
    visit(binding: ast[i].pattern)
    ast[i].initializer?.accept(&self)
  }

  mutating func visit(conformance i: NodeIndex<ConformanceDecl>) {
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

  mutating func visit(extension i: NodeIndex<ExtensionDecl>) {
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

  mutating func visit(fun i: NodeIndex<FunDecl>) {
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

  mutating func visit(genericSizeParam i: NodeIndex<GenericSizeParamDecl>) {
    hierarchy.insert(decl: i, into: innermost!)
  }

  mutating func visit(genericTypeParam i: NodeIndex<GenericTypeParamDecl>) {
    hierarchy.insert(decl: i, into: innermost!)
  }

  mutating func visit(methodImpl i: NodeIndex<MethodImplDecl>) {
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

  mutating func visit(module i: NodeIndex<ModuleDecl>) {
    innermost = AnyNodeIndex(i)
    for member in ast[i].members {
      member.accept(&self)
    }
  }

  mutating func visit(namespace i: NodeIndex<NamespaceDecl>) {
    hierarchy.insert(decl: i, into: innermost!)
    nesting(in: i, { this in
      for member in this.ast[i].members {
        member.accept(&this)
      }
    })
  }

  mutating func visit(param i: NodeIndex<ParamDecl>) {
    hierarchy.insert(decl: i, into: innermost!)
    ast[i].annotation?.accept(&self)
    ast[i].defaultValue?.accept(&self)
  }

  mutating func visit(productType i: NodeIndex<ProductTypeDecl>) {
    hierarchy.insert(decl: i, into: innermost!)
    nesting(in: i, { this in
      this.visit(genericClause: this.ast[i].genericClause?.value)
      for member in this.ast[i].members {
        member.accept(&this)
      }
    })
  }

  mutating func visit(subscript i: NodeIndex<SubscriptDecl>) {
    hierarchy.insert(decl: i, into: innermost!)
    nesting(in: i, { this in
      let decl = this.ast[i]
      for capture in decl.captures {
        this.visit(binding: capture)
      }

      for param in decl.parameters {
        this.visit(param: param)
      }

      decl.output.accept(&this)

      for impl in decl.impls {
        this.visit(subscriptImpl: impl)
      }
    })
  }

  mutating func visit(subscriptImpl i: NodeIndex<SubscriptImplDecl>) {
    hierarchy.insert(decl: i, into: innermost!)
    ast[i].body.map({ stmt in visit(brace: stmt) })
  }

  mutating func visit(trait i: NodeIndex<TraitDecl>) {
    hierarchy.insert(decl: i, into: innermost!)
    nesting(in: i, { this in
      for member in this.ast[i].members {
        member.accept(&this)
      }
    })
  }

  mutating func visit(typeAlias i: NodeIndex<TypeAliasDecl>) {
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

  mutating func visit(`var` i: NodeIndex<VarDecl>) {
    hierarchy.insert(decl: i, into: innermost!)
  }

  // MARK: Expressions

  mutating func visit(async i: NodeIndex<AsyncExpr>) {
    visit(fun: ast[i].decl)
  }

  mutating func visit(await i: NodeIndex<AwaitExpr>) {
    ast[i].operand.accept(&self)
  }

  mutating func visit(boolLiteral i: NodeIndex<BoolLiteralExpr>) {}

  mutating func visit(bufferLiteral i: NodeIndex<BufferLiteralExpr>) {
    for elem in ast[i].elements {
      elem.accept(&self)
    }
  }

  mutating func visit(charLiteral i: NodeIndex<CharLiteralExpr>) {}

  mutating func visit(cond i: NodeIndex<CondExpr>) {
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

  mutating func visit(floatLiteral i: NodeIndex<FloatLiteralExpr>) {}

  mutating func visit(funCall i: NodeIndex<FunCallExpr>) {
    ast[i].callee.accept(&self)
    for arg in ast[i].arguments {
      arg.value.value.accept(&self)
    }
  }

  mutating func visit(intLiteral i: NodeIndex<IntLiteralExpr>) {}

  mutating func visit(lambda i: NodeIndex<LambdaExpr>) {
    visit(fun: ast[i].decl)
  }

  mutating func visit(mapLiteral i: NodeIndex<MapLiteralExpr>) {
    for elem in ast[i].elements {
      elem.value.key.accept(&self)
      elem.value.value.accept(&self)
    }
  }

  mutating func visit(match i: NodeIndex<MatchExpr>) {
    ast[i].subject.accept(&self)

    for elem in ast[i].cases {
      visit(matchCase: elem)
    }
  }

  mutating func visit(matchCase i: NodeIndex<MatchCaseExpr>) {
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

  mutating func visit(name i: NodeIndex<NameExpr>) {
    if case let .explicit(domain) = ast[i].domain {
      domain.accept(&self)
    }

    for arg in ast[i].arguments {
      switch arg.value {
      case let .type(arg):
        arg.accept(&self)
      case let .size(arg):
        arg.accept(&self)
      }
    }
  }

  mutating func visit(nil i: NodeIndex<NilExpr>) {}

  mutating func visit(storedProjection i: NodeIndex<StoredProjectionExpr>) {
    ast[i].operand.accept(&self)
  }

  mutating func visit(stringLiteral i: NodeIndex<StringLiteralExpr>) {}

  mutating func visit(subscriptCall i: NodeIndex<SubscriptCallExpr>) {
    ast[i].callee.accept(&self)
    for arg in ast[i].arguments {
      arg.value.value.accept(&self)
    }
  }

  mutating func visit(tuple i: NodeIndex<TupleExpr>) {
    for elem in ast[i].elements {
      elem.value.value.accept(&self)
    }
  }

  mutating func visit(unfolded i: NodeIndex<UnfoldedExpr>) {
    for expr in ast[i].subexpressions {
      expr.accept(&self)
    }
  }

  // MARK: Patterns

  mutating func visit(binding i: NodeIndex<BindingPattern>) {
    ast[i].subpattern.accept(&self)
    ast[i].annotation?.accept(&self)
  }

  mutating func visit(expr i: NodeIndex<ExprPattern>) {
    ast[i].expr.accept(&self)
  }

  mutating func visit(name i: NodeIndex<NamePattern>) {}

  mutating func visit(tuple i: NodeIndex<TuplePattern>) {
    for elem in ast[i].elements {
      elem.value.pattern.accept(&self)
    }
  }

  mutating func visit(wildcard i: NodeIndex<WildcardPattern>) {}

  // MARK: Statements

  mutating func visit(brace i: NodeIndex<BraceStmt>) {
    nesting(in: i, { this in
      for stmt in this.ast[i].stmts {
        stmt.accept(&this)
      }
    })
  }

  mutating func visit(break i: NodeIndex<BreakStmt>) {}

  mutating func visit(continue i: NodeIndex<ContinueStmt>) {}

  mutating func visit(decl i: NodeIndex<DeclStmt>) {
    ast[i].decl.accept(&self)
  }

  mutating func visit(doWhile i: NodeIndex<DoWhileStmt>) {
    visit(brace: ast[i].body)

    // Visit the condition of the loop in the same lexical scope as the body.
    innermost = AnyNodeIndex(ast[i].body)
    ast[i].condition.accept(&self)
    innermost = hierarchy.parent[ast[i].body]
  }

  mutating func visit(expr i: NodeIndex<ExprStmt>) {
    ast[i].expr.accept(&self)
  }

  mutating func visit(for i: NodeIndex<ForStmt>) {
    nesting(in: i, { this in
      this.visit(binding: this.ast[i].binding)
      this.ast[i].filter?.accept(&this)
      this.visit(brace: this.ast[i].body)
    })
  }

  mutating func visit(return i: NodeIndex<ReturnStmt>) {
    ast[i].value?.accept(&self)
  }

  mutating func visit(while i: NodeIndex<WhileStmt>) {
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

  mutating func visit(yield i: NodeIndex<YieldStmt>) {
    ast[i].value.accept(&self)
  }

  // MARK: Type expressions

  mutating func visit(async i: NodeIndex<AsyncTypeExpr>) {
    ast[i].operand.accept(&self)
  }

  mutating func visit(conformanceLens i: NodeIndex<ConformanceLensTypeExpr>) {
    ast[i].base.accept(&self)
  }

  mutating func visit(existential i: NodeIndex<ExistentialTypeExpr>) {
    visit(whereClause: ast[i].whereClause?.value)
  }

  mutating func visit(indirect i: NodeIndex<IndirectTypeExpr>) {
    ast[i].operand.accept(&self)
  }

  mutating func visit(lambda i: NodeIndex<LambdaTypeExpr>) {
    ast[i].environment?.accept(&self)
    for param in ast[i].parameters {
      visit(param: param)
    }
    ast[i].output.accept(&self)
  }

  mutating func visit(name i: NodeIndex<NameTypeExpr>) {
    ast[i].domain?.accept(&self)

    for arg in ast[i].arguments {
      switch arg.value {
      case let .type(arg):
        arg.accept(&self)
      case let .size(arg):
        arg.accept(&self)
      }
    }
  }

  mutating func visit(param i: NodeIndex<ParamTypeExpr>) {
    ast[i].bareType.accept(&self)
  }

  mutating func visit(storedProjection i: NodeIndex<StoredProjectionTypeExpr>) {
    ast[i].operand.accept(&self)
  }

  mutating func visit(tuple i: NodeIndex<TupleTypeExpr>) {
    for elem in ast[i].elements {
      elem.value.type.accept(&self)
    }
  }

  mutating func visit(union i: NodeIndex<UnionTypeExpr>) {
    for elem in ast[i].elements {
      elem.accept(&self)
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
