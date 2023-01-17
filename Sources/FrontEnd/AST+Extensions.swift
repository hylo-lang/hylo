import Core
import Utils
import ValModule

extension AST {

  /// Imports the core library into `self`.
  ///
  /// - Requires: The Core library must not have been already imported.
  public mutating func importCoreModule() {
    precondition(!isCoreModuleLoaded, "Core library is already loaded")

    do {
      var diagnostics = Diagnostics()
      corelib = try makeModule(
        "Val", sourceCode: sourceFiles(in: [ValModule.core!]), diagnostics: &diagnostics)

      assert(isCoreModuleLoaded)
    } catch let error {
      fatalError("Error parsing the core module:\n\(error.localizedDescription)")
    }
  }

}
