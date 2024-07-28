import Foundation
import FrontEnd
import Utils

extension AST {

  /// Creates an instance that includes the Hylo library whose sources are rooted at `libraryRoot`.
  init(libraryRoot: URL, _ compilationConditions: ConditionalCompilationFactors) throws {
    self.init(compilationConditions)
    var diagnostics = DiagnosticSet()
    coreLibrary = try loadModule(
      "Hylo",
      parsing: sourceFiles(in: [libraryRoot]),
      withBuiltinModuleAccess: true,
      reportingDiagnosticsTo: &diagnostics)
    assert(coreModuleIsLoaded)
    self.coreTraits = .init(self)
  }

}
