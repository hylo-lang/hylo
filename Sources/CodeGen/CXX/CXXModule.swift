import Core
import FrontEnd
import Utils

/// A C++ module.
public struct CXXModule {

  /// The module's declaration in Val's AST.
  let source: ModuleDecl.Typed

  /// The C++ top-level declarations for this module
  let topLevelDecls: [CXXTopLevelDecl]

  /// The module's name.
  var name: String {
    source.baseName
  }

}
