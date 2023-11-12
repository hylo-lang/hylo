import Core
import Foundation
import HyloModule
import Utils

extension AST {

  /// An instance that includes just the core module.
  public static var coreModule = AST(libraryRoot: HyloModule.core)

  /// An instance that includes just the standard library.
  public static var standardLibrary = AST(libraryRoot: HyloModule.standardLibrary)

  /// Creates an instance that includes the Hylo library rooted at `libraryRoot`.
  private init(libraryRoot: URL) {
    self.init()
    do {
      var diagnostics = DiagnosticSet()
      coreLibrary = try makeModule(
        "Hylo",
        sourceCode: sourceFiles(in: [libraryRoot]),
        builtinModuleAccess: true,
        diagnostics: &diagnostics)
      assert(isCoreModuleLoaded)
      self.coreTraits = .init(self)
    } catch let error {
      fatalError("Error parsing the Hylo module:\n\(error)")
    }
  }

}
