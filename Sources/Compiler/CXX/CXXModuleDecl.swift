/// A C++ module.
public struct CXXModuleDecl : CXXDecl {

  /// The corresponding Val AST node
  let original: ModuleDecl

  /// The top-level declarations in the module
  var topLevelDecsls: [CXXDecl] = []

  /// Emits the C++ header file content representing our module.
  public func emitHeader() -> String {
    var output: String = ""

    // Emit the header guard.
    output.write("#pragma once\n")
    output.write("\n")

    // Emit include clauses.
    output.write("#include <utility>\n")
    output.write("#include <functional>\n")
    output.write("\n")

    // Create a namespace for the entire module.
    output.write("namespace \(original.name) {\n")

    // TODO: translate top-level declarationsd

    output.write("}\n") // module namespace

    return output
  }

  /// Emits the C++ implementation file content representing our module.
  public func emitImplementation() -> String {
    var output: String = ""

    // Emit include clauses.
    output.write("#include \"\(original.name).hpp\"\n")
    output.write("\n")

    // Create a namespace for the entire module.
    output.write("namespace \(original.name) {\n")

    // TODO: translate top-level declaration implementations

    output.write("}\n") // module namespace

    return output
  }

}
