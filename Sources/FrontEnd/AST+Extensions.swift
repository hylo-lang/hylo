import Core
import Utils
import ValModule

extension AST {

  /// An instance that includes the core module.
  public static var coreModule: AST = {
    var r = AST()
    do {
      var diagnostics = Diagnostics()
      r.corelib = try r.makeModule(
        "Val",
        sourceCode: sourceFiles(in: [ValModule.core!]),
        builtinModuleAccess: true,
        diagnostics: &diagnostics)

      assert(r.isCoreModuleLoaded)
    } catch let error {
      fatalError("Error parsing the core module:\n\(error.localizedDescription)")
    }

    return r
  }()

}
