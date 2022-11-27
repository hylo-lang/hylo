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
  public mutating func emit(module decl: NodeID<ModuleDecl>) -> CXXModule {
    var module = CXXModule(valDecl: decl, name: program.ast[decl].name)
    for member in program.ast.topLevelDecls(decl) {
      emit(topLevel: member, into: &module)
    }
    return module
  }

  /// Emits the given top-level declaration into `module`.
  public mutating func emit(topLevel decl: AnyDeclID, into module: inout CXXModule) {
    switch decl.kind {
    case FunctionDecl.self:
      emit(function: NodeID(rawValue: decl.rawValue), into: &module)
    default:
      unreachable("unexpected declaration")
    }
  }

  /// Emits the given function declaration into `module`.
  public mutating func emit(function decl: NodeID<FunctionDecl>, into module: inout CXXModule) {
    // Declare the function in the module if necessary.
    let valFunctionDecl = TypedProgram.Node<FunctionDecl>(whole: program, id: decl)
    let id = module.getOrCreateFunction(correspondingTo: valFunctionDecl)

    // If we have a body for our function, emit it.
    if let body = program.ast[decl].body {
      module.setFunctionBody(emit(funBody: body), forID: id)
    }
  }

  /// Translate the function body into a CXX entity.
  private mutating func emit(funBody body: FunctionDecl.Body) -> CXXRepresentable {
    switch body {
    case .block(let stmt):
      return emit(brace: stmt)

    case .expr:
      let exprStmt = CXXComment(comment: "expr")
      return CXXScopedBlock(stmts: [exprStmt])
    }
  }

  // MARK: Declarations

  private mutating func emit(localBinding decl: NodeID<BindingDecl>) -> CXXRepresentable {
    let pattern = program.ast[decl].pattern

    switch program.ast[pattern].introducer.value {
    case .var, .sinklet:
      return emit(storedLocalBinding: decl)
    case .let:
      return emit(borrowedLocalBinding: decl, withCapability: .let)
    case .inout:
      return emit(borrowedLocalBinding: decl, withCapability: .inout)
    }
  }

  private mutating func emit(storedLocalBinding decl: NodeID<BindingDecl>) -> CXXRepresentable {
    return CXXComment(comment: "local binding")
  }

  /// Emits borrowed bindings.
  private mutating func emit(
    borrowedLocalBinding decl: NodeID<BindingDecl>,
    withCapability capability: RemoteType.Capability
  ) -> CXXRepresentable {
    // There's nothing to do if there's no initializer.
    if let initializer: AnyExprID = program.ast[decl].initializer {

      let isLValue = (initializer.kind == NameExpr.self) || (initializer.kind == SubscriptCallExpr.self)

      // Visit the initializer.
      let cxxInitialzer = emit(initializer, asLValue: isLValue)

      // Visit the patterns.
      var stmts: [CXXRepresentable] = []
      let pattern = program.ast[decl].pattern
      for (path, name) in program.ast.names(in: program.ast[pattern].subpattern) {
        // TODO: emit code for the patterns.
        let decl = program.ast[name].decl
        let declType = program.declTypes[decl]!
        stmts.append(CXXComment(comment: "decl \(name), type: \(declType.description); path: \(path)"))
      }
      if stmts.isEmpty {
        // No pattern found; just call the initializer, dropping the result.
        return CXXVoidCast(baseExpr: cxxInitialzer)
      } else {
        return CXXScopedBlock(stmts: stmts)
      }
    }
    else {
      return CXXComment(comment: "EMPTY borrowed local binding (\(capability))")
    }
  }

  // MARK: Statements

  /// Emits the given statement into `module` at the current insertion point.
  private mutating func emit<T: StmtID>(stmt: T) -> CXXRepresentable {
    switch stmt.kind {
    case BraceStmt.self:
      return emit(brace: NodeID(rawValue: stmt.rawValue))
    case DeclStmt.self:
      return emit(declStmt: NodeID(rawValue: stmt.rawValue))
    case ExprStmt.self:
      return emit(exprStmt: NodeID(rawValue: stmt.rawValue))
    case ReturnStmt.self:
      return emit(returnStmt: NodeID(rawValue: stmt.rawValue))
    default:
      unreachable("unexpected statement")
    }
  }

  private mutating func emit(brace stmt: NodeID<BraceStmt>) -> CXXRepresentable {
    var stmts: [CXXRepresentable] = []
    for s in program.ast[stmt].stmts {
      stmts.append(emit(stmt: s))
    }
    return CXXScopedBlock(stmts: stmts)
  }

  private mutating func emit(declStmt stmt: NodeID<DeclStmt>) -> CXXRepresentable {
    switch program.ast[stmt].decl.kind {
    case BindingDecl.self:
      return emit(localBinding: NodeID(rawValue: program.ast[stmt].decl.rawValue))
    default:
      unreachable("unexpected declaration")
    }
  }

  private mutating func emit(exprStmt stmt: NodeID<ExprStmt>) -> CXXRepresentable {
    return CXXComment(comment: "expr stmt")
  }

  private mutating func emit(returnStmt stmt: NodeID<ReturnStmt>) -> CXXRepresentable {
    return CXXComment(comment: "return stmt")
  }

  // MARK: Expressions

  private mutating func emit(
    _ expr: AnyExprID,
    asLValue: Bool
  ) -> CXXRepresentable {
    if asLValue {
      return CXXComment(comment: "expr (lvalue)")
    } else {
      return CXXComment(comment: "expr (rvalue)")
    }
  }

}
