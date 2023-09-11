import Core
import Foundation
import HyloModule
import Utils

extension AST {

  /// An instance that includes just the core module.
  public static var coreModule = AST(standardLibraryPathSuffix: "Hylo/Core")

  /// An instance that includes just the standard library.
  public static var standardLibrary = AST(standardLibraryPathSuffix: "Hylo")

  /// Creates an instance that includes the Hylo standard library components under the given path
  /// suffix (both plain source and files generated from .gyb).
  private init(standardLibraryPathSuffix: String) {
    self.init()
    do {
      var diagnostics = DiagnosticSet()
      coreLibrary = try makeModule(
        "Hylo",
        sourceCode: sourceFiles(
          in: [
            standardLibraryRoot.appendingPathComponent(standardLibraryPathSuffix),
            standardLibraryBundleRoot.appendingPathComponent(standardLibraryPathSuffix)
          ]),
        builtinModuleAccess: true,
        diagnostics: &diagnostics)
      assert(isCoreModuleLoaded)
    } catch let error {
      fatalError("Error parsing the Hylo module:\n\(error)")
    }
  }

}
