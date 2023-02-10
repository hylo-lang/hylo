import Core
import Utils
import ValModule

extension AST {

  /// Incorporates syntax for the core library into `self`.
  ///
  /// - Requires: The Core library must not have been already imported.
  public mutating func importCoreModule() {
    precondition(!isCoreModuleLoaded, "Core library is already loaded")

    do {
      var diagnostics = Diagnostics()
      corelib = try makeModule(
        "Val",
        sourceCode: sourceFiles(in: [ValModule.core!]),
        builtinModuleAccess: true,
        diagnostics: &diagnostics)

      assert(isCoreModuleLoaded)
    } catch let error {
      fatalError("Error parsing the core module:\n\(error.localizedDescription)")
    }
  }

  /// Returns `self`, with syntax for the core module included if it is not there already.
  public func importingCoreModule() -> Self {
    if isCoreModuleLoaded { return self }
    var r = self
    r.importCoreModule()
    return r
  }

}
