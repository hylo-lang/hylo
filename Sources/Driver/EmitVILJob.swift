import AST
import VIL

/// An job that lowers a type checked module declaration to VIL.
public struct EmitVILJob: Job {

  /// The unique identifier of the module to lower.
  public var moduleID: String

  public init(moduleID: String) {
    self.moduleID = moduleID
  }

  public func run(in context: Context) throws {
    // Retrieve the module to lower.
    guard let moduleDecl = context.modules[moduleID] else {
      throw DriverError.moduleNotFound(moduleID: moduleID)
    }

    // Initialize the VIL emitter.
    let module = Module(id: moduleID)
    let builder = VIL.Builder(module: module)
    let emitter = Emitter(builder: builder)

    // Emit each declaration in the module.
    for decl in moduleDecl.decls {
      emitter.emit(decl: decl)
    }
  }

}
