/// A C++ top-level declaration that needs to be written both to headers and source files.
public protocol CXXTopLevelDecl {

  /// Write into target the CXX code that needs to reach the header of the module.
  func writeDeclaration<Target: TextOutputStream>(into target: inout Target)

  /// Write into target the CXX code corresponding to the definition of `self`.
  func writeDefinition<Target: TextOutputStream>(into target: inout Target)

}
