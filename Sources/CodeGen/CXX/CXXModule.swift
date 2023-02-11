import Core
import FrontEnd
import Utils

/// A C++ module.
public struct CXXModule {

  /// The module's name
  public let name: String

  /// True if this represents C++ translation of Val standard library.
  let isStdLib: Bool

  /// The C++ top-level declarations for this module
  let topLevelDecls: [CXXTopLevelDecl]

  /// The body of this module's entry point (i.e., the `main` function), if any.
  let entryPointBody: CXXStmt?

}
