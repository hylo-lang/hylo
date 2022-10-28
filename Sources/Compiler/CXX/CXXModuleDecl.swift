/// A C++ module.
public struct CXXModuleDecl : CXXDecl {

  /// The module's declaration.
  public let decl: NodeID<ModuleDecl>

  /// The module's name.
  public let name: String

  /// The top-level declarations in the module
  var topLevelDecls: [CXXDecl] = []

  public init(decl: NodeID<ModuleDecl>, name: String) {
    self.decl = decl
    self.name = name
  }

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
    output.write("namespace \(name) {\n")

    // TODO: translate top-level declarationsd

    output.write("}\n") // module namespace

    return output
  }

  /// Emits the C++ implementation file content representing our module.
  public func emitImplementation() -> String {
    var output: String = ""

    // Emit include clauses.
    output.write("#include \"\(name).h\"\n")
    output.write("\n")

    // Create a namespace for the entire module.
    output.write("namespace \(name) {\n")

    // TODO: translate top-level declaration implementations

    output.write("}\n") // module namespace

    return output
  }

}
