import Core
import Foundation
import StandardLibrary
import Utils

extension AST {

  /// Creates an instance that includes the Hylo library whose sources are rooted at `libraryRoot`.
  public init(
    libraryRoot: URL,
    _ factors: ConditionalCompilationFactors = ConditionalCompilationFactors()
  ) {
    self.init(factors)
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
