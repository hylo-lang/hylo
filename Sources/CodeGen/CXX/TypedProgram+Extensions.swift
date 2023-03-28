import Core

extension TypedProgram {

  /// The bundle of products resulting from transpiling a module to C++.
  public typealias CXXModule = (syntax: CodeGenCXX.CXXModule, text: TranslationUnitCode)

  /// Returns the C++ Transpilation of `m`.
  public func cxx(_ m: ModuleDecl.Typed, withFormatter formatter: CodeTransform? = nil) -> CXXModule
  {
    let t = CXXTranspiler(self)
    let x = t.cxx(m)
    return (syntax: x, text: x.code(formatter: formatter))
  }

}
