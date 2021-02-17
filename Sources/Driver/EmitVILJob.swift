import AST
import VIL

/// A job that lowers a type checked module declaration to VIL.
///
/// This job should typcially be run after a `CompileJob`. It will consume all module declarations
/// on top of the driver's stack and produce a VIL module for each of them.
public struct EmitVILJob: Job {

  public init() {
  }

  public func run(with driver: inout Driver) throws {
    var results: [Module] = []

    while let moduleDecl = driver.stack.last as? ModuleDecl {
      driver.stack.removeLast()
      guard moduleDecl.state == .typeChecked else { continue }

      // Initialize the VIL emitter.
      let module = Module(id: moduleDecl.id)
      let builder = VIL.Builder(module: module)
      let emitter = Emitter(context: driver.context, builder: builder)

      // Emit the module declaration.
      emitter.emit(moduleDecl: moduleDecl)
      module.dump()

      results.append(module)
    }

    driver.stack.append(contentsOf: results.reversed())
  }

}
