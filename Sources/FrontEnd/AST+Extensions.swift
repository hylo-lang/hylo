import Core
import Foundation
import Utils

extension AST {

  /// Creates an instance representing the standard library built from `standardLibrarySources`.
  public init<S: Sequence>(standardLibrarySources: S) throws
    where S.Element == SourceFile
  {
    self.init()
    var diagnostics = DiagnosticSet()

    coreLibrary = try makeModule(
      "Hylo", sourceCode: standardLibrarySources,
      builtinModuleAccess: true, diagnostics: &diagnostics)

    assert(isCoreModuleLoaded)
    self.coreTraits = .init(self)
  }

}
