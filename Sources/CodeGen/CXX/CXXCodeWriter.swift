/// Class used to write the output CXX code from the given CXX AST.
public struct CXXCodeWriter {

  /// Initializes the current object.
  public init() {}

  // MARK: API

  /// Write the CXX header content for the given module to the given text stream.
  public func emitHeaderCode(_ module: CXXModule) -> String {
    var target: String = ""

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
      writeInterface(topLevel: decl, into: &target)
    }

    target.write("\n}\n")  // module namespace

    return target
  }

  /// Write the CXX source content for the given module to the given text stream.
  public func emitSourceCode(_ module: CXXModule) -> String {
    var target: String = ""

    // Emit include clauses.
    target.write("#include \"\(module.valDecl.name).h\"\n")
    target.write("\n")

    // Create a namespace for the entire module.
    target.write("namespace \(module.valDecl.name) {\n\n")

    // Emit the C++ text needed for the source file corresponding to the C++ declarations.
    for decl in module.cxxTopLevelDecls {
      writeDefinition(topLevel: decl, into: &target)
    }

    target.write("\n}\n")  // module namespace

    return target
  }

  // MARK: Top-level declarations

  private func writeInterface(topLevel decl: CXXTopLevelDecl, into target: inout String) {
    switch type(of: decl).kind {
    case CXXFunctionDecl.self:
      writeSignature(function: decl as! CXXFunctionDecl, into: &target)
    case CXXClassDecl.self:
      writeSignature(type: decl as! CXXClassDecl, into: &target)
    default:
      fatalError("unexpected top-level declaration")
    }
    target.write(";\n")
  }

  private func writeDefinition(topLevel decl: CXXTopLevelDecl, into target: inout String) {
    switch type(of: decl).kind {
    case CXXFunctionDecl.self:
      writeDefinition(function: decl as! CXXFunctionDecl, into: &target)
    case CXXClassDecl.self:
      writeDefinition(type: decl as! CXXClassDecl, into: &target)
    default:
      fatalError("unexpected top-level declaration")
    }
  }

  private func writeSignature(function decl: CXXFunctionDecl, into target: inout String) {
    target.write("\(decl.output) \(decl.identifier)(")
    for i in 0 ..< decl.parameters.count {
      if i != 0 { target.write(", ") }
      target.write("\(decl.parameters[i].type) \(decl.parameters[i].name)")
    }
    target.write(")")
  }
  private func writeDefinition(function decl: CXXFunctionDecl, into target: inout String) {
    writeSignature(function: decl, into: &target)
    if decl.body != nil {
      target.write(" ")
      decl.body!.writeCode(into: &target)
    } else {
      target.write(";\n")
    }
  }

  private func writeSignature(type decl: CXXClassDecl, into target: inout String) {
    target.write("class \(decl.name)")
  }
  private func writeDefinition(type decl: CXXClassDecl, into target: inout String) {
    writeSignature(type: decl, into: &target)
    target.write(" {\n")
    target.write("public:\n")
    for member in decl.members {
      switch member {
      case .attribute(let attribute):
        attribute.writeCode(into: &target)
      case .method:
        target.write("// method\n")
      case .constructor:
        target.write("// constructor\n")
      }
    }
    target.write("};\n")
  }

}
