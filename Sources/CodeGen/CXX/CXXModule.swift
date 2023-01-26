import Core
import FrontEnd
import Utils

/// A C++ module.
public struct CXXModule {

  /// The module's declaration in Val's AST.
  let valDecl: ModuleDecl.Typed

  /// The typed program for wich we are constructing the CXX translation.
  let program: TypedProgram

  /// The C++ top-level declarations for this module
  private(set) var cxxTopLevelDecls: [CXXTopLevelDecl] = []

  init(_ decl: ModuleDecl.Typed, for program: TypedProgram) {
    self.valDecl = decl
    self.program = program
  }

  /// Add a top-level C++ declaration to this module.
  mutating func addTopLevelDecl(_ decl: CXXTopLevelDecl) {
    cxxTopLevelDecls.append(decl)
  }

}
