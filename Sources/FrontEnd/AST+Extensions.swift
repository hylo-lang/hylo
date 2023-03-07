import Core
import Utils
import ValModule

extension AST {

  /// An instance that includes just the core module.
  public static var coreModule = AST(withCoreModule: ())

  /// Creates an instance that includes just the core module.
  private init(withCoreModule: Void) {
    self = AST()
    do {
      var diagnostics = DiagnosticSet()
      coreLibrary = try makeModule(
        "Val",
        sourceCode: sourceFiles(in: [ValModule.core!]),
        builtinModuleAccess: true,
        diagnostics: &diagnostics)

      assert(isCoreModuleLoaded)
    } catch let error {
      fatalError("Error parsing the core module:\n\(error.localizedDescription)")
    }
  }

}
