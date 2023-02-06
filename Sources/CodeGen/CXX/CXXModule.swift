import Core
import FrontEnd
import Utils

/// A C++ module.
public struct CXXModule {

  /// The module's declaration in Val's AST.
  let source: ModuleDecl.Typed

  /// The C++ top-level declarations for this module
  let topLevelDecls: [CXXTopLevelDecl]

  /// The body of this module's entry point (i.e., the `main` function), if any.
  let entryPointBody: CXXStmt?

  /// The module's name.
  var name: String {
    source.baseName
  }

}
