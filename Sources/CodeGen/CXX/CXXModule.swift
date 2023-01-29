import Core
import FrontEnd
import Utils

/// A C++ module.
public struct CXXModule {

  /// The module's declaration in Val's AST.
  let valDecl: ModuleDecl.Typed

  /// The C++ top-level declarations for this module
  private(set) var cxxTopLevelDecls: [CXXTopLevelDecl] = []

  init(_ decl: ModuleDecl.Typed) {
    self.valDecl = decl
  }

  /// Add a top-level C++ declaration to this module.
  mutating func addTopLevelDecl(_ decl: CXXTopLevelDecl) {
    cxxTopLevelDecls.append(decl)
  }

}
