import Utils

/// A Val to C++ transpiler.
public struct CXXTranspiler {

  /// The program being transpiled.
  public let program: TypedProgram

  /// Creates a C++ transpiler with a well-typed AST.
  public init(program: TypedProgram) {
    self.program = program
  }

  // MARK: API

  /// Emits the C++ module corresponding to the Val module identified by `decl`.
  public mutating func emit(module decl: Typed<ModuleDecl>) -> CXXModule {
    var module = CXXModule(decl, for: program)
    for member in decl.topLevelDecls {
      emit(topLevel: member, into: &module)
    }
    return module
  }

  /// Emits the given top-level declaration into `module`.
  mutating func emit(topLevel decl: TypedNode<AnyDeclID>, into module: inout CXXModule) {
    switch decl.kind {
    case FunctionDecl.self:
      emit(function: Typed<FunctionDecl>(decl)!, into: &module)
    default:
      unreachable("unexpected declaration")
    }
  }

  /// Emits the given function declaration into `module`.
  public mutating func emit(function decl: Typed<FunctionDecl>, into module: inout CXXModule) {
    // Declare the function in the module if necessary.
    let id = module.getOrCreateFunction(correspondingTo: decl)

    // If we have a body for our function, emit it.
    if let body = decl.body {
      module.setFunctionBody(emit(funBody: body), forID: id)
    }
  }

  /// Translate the function body into a CXX entity.
  private mutating func emit(funBody body: Typed<FunctionDecl>.Body) -> CXXRepresentable {
    switch body {
    case .block(let stmt):
      return emit(brace: stmt)

    case .expr:
      let exprStmt = CXXComment(comment: "expr")
      return CXXScopedBlock(stmts: [exprStmt])
    }
  }

  // MARK: Declarations

  private mutating func emit(localBinding decl: Typed<BindingDecl>) -> CXXRepresentable {
    let pattern = decl.pattern

    switch pattern.introducer.value {
    case .var, .sinklet:
      return emit(storedLocalBinding: decl)
    case .let:
      return emit(borrowedLocalBinding: decl, withCapability: .let)
    case .inout:
      return emit(borrowedLocalBinding: decl, withCapability: .inout)
    }
  }

  private mutating func emit(storedLocalBinding decl: Typed<BindingDecl>) -> CXXRepresentable {
    return CXXComment(comment: "local binding")
  }

  /// Emits borrowed bindings.
  private mutating func emit(
    borrowedLocalBinding decl: Typed<BindingDecl>, withCapability capability: RemoteType.Capability
  ) -> CXXRepresentable {
    // There's nothing to do if there's no initializer.
    if let initializer = decl.initializer {

      let isLValue =
        (initializer.kind == NameExpr.self) || (initializer.kind == SubscriptCallExpr.self)

      // Visit the initializer.
      let cxxInitialzer = emit(expr: initializer, asLValue: isLValue)

      // Visit the patterns.
      var stmts: [CXXRepresentable] = []
      let pattern = decl.pattern
      for (path, name) in pattern.subpattern.names {
        // TODO: emit code for the patterns.
        let decl = name.decl
        stmts.append(
          CXXComment(comment: "decl \(name), type: \(decl.type.description); path: \(path)"))
      }
      if stmts.isEmpty {
        // No pattern found; just call the initializer, dropping the result.
        return CXXVoidCast(baseExpr: cxxInitialzer)
      } else {
        return CXXScopedBlock(stmts: stmts)
      }
    } else {
      return CXXComment(comment: "EMPTY borrowed local binding (\(capability))")
    }
  }

  // MARK: Statements

  /// Emits the given statement into `module` at the current insertion point.
  private mutating func emit<ID: StmtID>(stmt: TypedNode<ID>) -> CXXRepresentable {
    switch stmt.kind {
    case BraceStmt.self:
      return emit(brace: Typed<BraceStmt>(stmt)!)
    case DeclStmt.self:
      return emit(declStmt: Typed<DeclStmt>(stmt)!)
    case ExprStmt.self:
      return emit(exprStmt: Typed<ExprStmt>(stmt)!)
    case ReturnStmt.self:
      return emit(returnStmt: Typed<ReturnStmt>(stmt)!)
    default:
      unreachable("unexpected statement")
    }
  }

  private mutating func emit(brace stmt: Typed<BraceStmt>) -> CXXRepresentable {
    var stmts: [CXXRepresentable] = []
    for s in stmt.stmts {
      stmts.append(emit(stmt: s))
    }
    return CXXScopedBlock(stmts: stmts)
  }

  private mutating func emit(declStmt stmt: Typed<DeclStmt>) -> CXXRepresentable {
    switch stmt.decl.kind {
    case BindingDecl.self:
      return emit(localBinding: Typed<BindingDecl>(stmt.decl)!)
    default:
      unreachable("unexpected declaration")
    }
  }

  private mutating func emit(exprStmt stmt: Typed<ExprStmt>) -> CXXRepresentable {
    return CXXComment(comment: "expr stmt")
  }

  private mutating func emit(returnStmt stmt: Typed<ReturnStmt>) -> CXXRepresentable {
    return CXXComment(comment: "return stmt")
  }

  // MARK: Expressions

  private mutating func emit(expr: TypedNode<AnyExprID>, asLValue: Bool) -> CXXRepresentable {
    if asLValue {
      return CXXComment(comment: "expr (lvalue)")
    } else {
      return CXXComment(comment: "expr (rvalue)")
    }
  }

}
