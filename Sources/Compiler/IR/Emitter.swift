import Utils

/// Val's IR emitter.
///
/// The emitter transforms well-formed, typed ASTs to a representation suitable for flow-sensitive
/// analysis and code generation.
public struct Emitter {

  /// The AST being lowered.
  public let ast: AST

  public init(ast: AST) {
    self.ast = ast
  }

  /// Emits the VIR of the module identified by `decl`.
  public mutating func emit(module decl: NodeID<ModuleDecl>) -> Module {
    var module = Module(id: ast[decl].name)
    for member in ast[decl].members {
      emit(topLevel: member, into: &module)
    }
    return module
  }

  /// Emits the given top-level declaration into `module`.
  public mutating func emit(topLevel decl: AnyDeclID, into module: inout Module) {
    switch decl.kind {
    case .funDecl:
      emit(fun: NodeID(unsafeRawValue: decl.rawValue), into: &module)
    default:
      unreachable("unexpected declaration")
    }
  }

  /// Emits the given function declaration into `module`.
  public mutating func emit(fun decl: NodeID<FunDecl>, into module: inout Module) {
    // Declare the function in the module if necessary.
    let functionID = module.getOrCreateFunction(from: decl, ast: ast)

    print(functionID)
  }

}
