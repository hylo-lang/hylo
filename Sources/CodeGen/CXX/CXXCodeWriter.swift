/// Class used to write the output CXX code from the given CXX AST.
public struct CXXCodeWriter {

  /// Initializes the current object.
  public init() {}

  /// Write the CXX header content for the given module to the given text stream.
  public func writeHeaderCode<Target: TextOutputStream>(
    _ module: CXXModule, into target: inout Target
  ) {
    // Emit the header guard.
    // "#pragma once" is non-standard, but implemented by all major compilers,
    // and it typically does a better job
    // (more efficiently treated in the compiler, and reduces probability of accidents)
    target.write("#pragma once\n")
    target.write("\n")

    // Emit include clauses.
    target.write("#include <variant>\n")
    target.write("\n")

    // Create a namespace for the entire module.
    target.write("namespace \(module.valDecl.name) {\n\n")

    // Emit the C++ text needed for the header corresponding to the C++ declarations.
    for decl in module.cxxTopLevelDecls {
      decl.writeDeclaration(into: &target)
    }

    target.write("\n}\n")  // module namespace
  }

  /// Write the CXX source content for the given module to the given text stream.
  public func writeSourceCode<Target: TextOutputStream>(
    _ module: CXXModule, into target: inout Target
  ) {
    // Emit include clauses.
    target.write("#include \"\(module.valDecl.name).h\"\n")
    target.write("\n")

    // Create a namespace for the entire module.
    target.write("namespace \(module.valDecl.name) {\n\n")

    // Emit the C++ text needed for the source file corresponding to the C++ declarations.
    for decl in module.cxxTopLevelDecls {
      decl.writeDefinition(into: &target)
    }

    target.write("\n}\n")  // module namespace
  }

}
