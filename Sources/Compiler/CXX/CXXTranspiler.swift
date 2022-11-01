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
    case .funDecl:
      emit(fun: NodeID(rawValue: decl.rawValue), into: &module)
    default:
      unreachable("unexpected declaration")
    }
  }

  /// Emits the given function declaration into `module`.
  public mutating func emit(fun decl: NodeID<FunDecl>, into module: inout CXXModule) {
    // Declare the function in the module if necessary.
    _ = module.getOrCreateFunction(correspondingTo: decl, program: program)
  }

}
