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

  /// Returns a deserialization of a serialization of `self` (i.e. a copy of `self`).
  func roundTripSerialized() throws -> Self {
    try JSONDecoder().forAST.decode(AST.self, from: JSONEncoder().forAST.encode(self))
  }
}
