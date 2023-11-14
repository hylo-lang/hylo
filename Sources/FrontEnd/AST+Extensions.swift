import Core
import Foundation
import Utils

extension AST {

  /// Creates an instance that includes the Hylo library whose sources are rooted at `libraryRoot`.
  public init(libraryRoot: URL) throws {
    self.init()
    var diagnostics = DiagnosticSet()
    coreLibrary = try makeModule(
      "Hylo",
      sourceCode: sourceFiles(in: [libraryRoot]),
      builtinModuleAccess: true,
      diagnostics: &diagnostics)
    assert(isCoreModuleLoaded)
    self.coreTraits = .init(self)
  }

}
