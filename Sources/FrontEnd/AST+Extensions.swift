import Core
import Utils
import ValModule

extension AST {

  /// An instance that includes just the core module.
  public static var coreModule: AST = {
    var a = AST()
    do {
      var diagnostics = DiagnosticSet()
      a.coreLibrary = try a.makeModule(
        "Val",
        sourceCode: sourceFiles(in: [ValModule.core!]),
        builtinModuleAccess: true,
        diagnostics: &diagnostics)
      assert(a.isCoreModuleLoaded)
    } catch let error {
      fatalError("Error parsing the core module:\n\(error.localizedDescription)")
    }
    return a
  }()

}
