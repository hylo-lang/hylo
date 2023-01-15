import Core
import FrontEnd
import Utils

/// A C++ module.
public struct CXXModule {

  /// The module's declaration in Val's AST.
  public let valDecl: ModuleDecl.Typed

  /// The typed program for wich we are constructing the CXX translation.
  public let program: TypedProgram

  /// The C++ top-level declarations for this module
  private var cxxTopLevelDecls: [CXXTopLevelDecl] = []

  public init(_ decl: ModuleDecl.Typed, for program: TypedProgram) {
    self.valDecl = decl
    self.program = program
  }

  /// Add a top-level C++ declaration to this module.
  public mutating func addTopLevelDecl(_ decl: CXXTopLevelDecl) {
    cxxTopLevelDecls.append(decl)
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

    // Emit the C++ text needed for the header corresponding to the C++ declarations.
    for decl in cxxTopLevelDecls {
      decl.writeDeclaration(into: &output)
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

    // Emit the C++ text needed for the source file corresponding to the C++ declarations.
    for decl in cxxTopLevelDecls {
      decl.writeDefinition(into: &output)
    }

    output.write("\n}\n")  // module namespace

    return output
  }

}
