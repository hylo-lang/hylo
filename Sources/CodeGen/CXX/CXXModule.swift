import Core
import FrontEnd
import Utils

/// A C++ module.
public struct CXXModule {

  /// The module's declaration in Val's AST.
  public let valDecl: ModuleDecl.Typed

  /// The typed program for wich we are constructing the CXX translation.
  public let program: TypedProgram

  /// The C++ functions declared in `self`.
  private var cxxFunctions: [CXXFunctionDecl] = []

  /// The C++ classes declared in this module.
  private var cxxClasses: [CXXClassDecl] = []

  public init(_ decl: ModuleDecl.Typed, for program: TypedProgram) {
    self.valDecl = decl
    self.program = program
  }

  /// Add a function to this module.
  public mutating func addFunction(_ functionDecl: CXXFunctionDecl) {
    cxxFunctions.append(functionDecl)
  }

  /// Add a class to this module.
  public mutating func addClass(_ classDecl: CXXClassDecl) {
    cxxClasses.append(classDecl)
  }

  // MARK: Serialization

  /// Emits the C++ header of the module.
  public func emitHeader() -> String {
    var output: String = ""

    // Emit the header guard.
    output.write("#ifndef VAL_\(valDecl.name.uppercased())_\n")
    output.write("#define VAL_\(valDecl.name.uppercased())_\n")
    output.write("\n")

    // Emit include clauses.
    output.write("#include <variant>\n")
    output.write("\n")

    // Create a namespace for the entire module.
    output.write("namespace \(valDecl.name) {\n\n")

    // Emit classes declarations.
    for decl in cxxClasses {
      decl.writeSignature(into: &output)
      output.write(";\n")
    }

    // Emit top-level functions.
    for decl in cxxFunctions {
      decl.writeSignature(into: &output)
      output.write(";\n")
    }

    output.write("\n}\n\n")  // module namespace
    output.write("#endif\n")  // header guard

    return output
  }

  /// Emits the C++ implementation of the module.
  public func emitSource() -> String {
    var output: String = ""

    // Emit include clauses.
    output.write("#include \"\(valDecl.name).h\"\n")
    output.write("\n")

    // Create a namespace for the entire module.
    output.write("namespace \(valDecl.name) {\n\n")

    // Emit classes definitions
    for decl in cxxClasses {
      decl.writeSignature(into: &output)
      output.write(" ")
      decl.writeDefinition(into: &output)
    }

    // Emit top-level functions.
    for decl in cxxFunctions {
      decl.writeSignature(into: &output)
      output.write(" ")
      if decl.body != nil {
        decl.body!.writeCode(into: &output)
      }
    }

    output.write("\n}\n")  // module namespace

    return output
  }

}
