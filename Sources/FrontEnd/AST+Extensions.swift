import Core
import Foundation
import HyloModule
import Utils

extension AST {

  /// Creates an instance that includes just the core module.
  public init(coreModuleFor compilerInfo: CompilerInfo) {
    self.init(libraryRoot: HyloModule.core, for: compilerInfo)
  }

  /// Creates an instance that includes just the standard library.
  public init(standardLibraryModuleFor compilerInfo: CompilerInfo) {
    self.init(libraryRoot: HyloModule.standardLibrary, for: compilerInfo)
  }

  /// Creates an instance that includes the Hylo library rooted at `libraryRoot`.
  private init(libraryRoot: URL, for compilerInfo: CompilerInfo) {
    self.init(for: compilerInfo)
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
