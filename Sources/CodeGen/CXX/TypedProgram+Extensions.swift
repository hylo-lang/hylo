import Core

extension TypedProgram {

  /// The bundle of products resulting from transpiling a module to C++.
  public typealias CXXModule = (syntax: CodeGenCXX.CXXModule, text: TranslationUnitCode)

  /// Returns the C++ Transpilation of `m`.
  public func cxx(_ m: ModuleDecl.Typed) -> CXXModule {
    let x = CXXTranspiler(self).cxx(m)
    var w = CXXCodeWriter()
    return (syntax: x, text: w.cxxCode(x))
  }

}
