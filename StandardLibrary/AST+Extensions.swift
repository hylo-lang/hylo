import Core
import Foundation
import FrontEnd
import Utils

extension AST {

  /// Creates an instance that includes the Hylo library whose sources are rooted at `libraryRoot`.
  init(libraryRoot: URL, for compiler: CompilerConfiguration) throws {
    self.init(for: compiler)
    var diagnostics = DiagnosticSet()
    coreLibrary = try makeModule(
      "Hylo",
      sourceCode: sourceFiles(in: [libraryRoot]),
      builtinModuleAccess: true,
      diagnostics: &diagnostics)
    assert(coreModuleIsLoaded)
    self.coreTraits = .init(self)
  }

}
