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

  /// Creates an instance representing `sourceCode` as a module named `moduleName`, with access to
  /// the builtin module iff `builtinModuleAccess` is `true`.
  public init<S: Sequence>(_ sourceCode: S, moduleName: String, builtinModuleAccess: Bool) throws where S.Element == URL {
    self.init()
    var diagnostics = DiagnosticSet()
    _ = try makeModule(moduleName, sourceCode: sourceCode, builtinModuleAccess: builtinModuleAccess, diagnostics: &diagnostics)
    self.coreTraits = .init(self)
  }
}
