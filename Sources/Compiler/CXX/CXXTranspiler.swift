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
    let id = module.getOrCreateFunction(correspondingTo: decl, program: program)

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
    return CXXComment(comment: "decl stmt")
  }

  private mutating func emit(exprStmt stmt: NodeID<ExprStmt>) -> CXXRepresentable {
    return CXXComment(comment: "expr stmt")
  }

  private mutating func emit(returnStmt stmt: NodeID<ReturnStmt>) -> CXXRepresentable {
    return CXXComment(comment: "return stmt")
  }



}
