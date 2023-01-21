/// A C++ top-level declaration that needs to be written both to headers and source files.
protocol CXXTopLevelDecl: CXXDecl {}

extension CXXTopLevelDecl {
  // TODO: remove me
  func writeCode<Target: TextOutputStream>(into target: inout Target) {}

}
