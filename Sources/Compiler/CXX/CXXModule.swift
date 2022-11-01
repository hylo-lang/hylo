import Utils

/// A C++ module.
public struct CXXModule {

  /// The module's declaration in Val's AST.
  public let valDecl: NodeID<ModuleDecl>

  /// The module's name.
  public let name: String

  /// The C++ functions declared in `self`.
  public private(set) var cxxFunctions: [CXXFunDecl] = []

  /// A table mapping val function declarations to the ID of the corresponding C++ declaration.
  private var valToCXXFunction: [NodeID<FunDecl>: Int] = [:]

  public init(valDecl: NodeID<ModuleDecl>, name: String) {
    self.valDecl = valDecl
    self.name = name
  }

  /// Returns the ID of the C++ function declaration corresponding to `valFunDecl`.
  ///
  /// - Requires: `valFunDecl` must be declared in `self.decl`.
  public mutating func getOrCreateFunction(
    correspondingTo valFunDecl: NodeID<FunDecl>,
    program: TypedProgram
  ) -> CXXFunDecl.ID {
    if let cxxFunDecl = valToCXXFunction[valFunDecl] { return cxxFunDecl }

    assert(program.scopeHierarchy.isGlobal(decl: valFunDecl, ast: program.ast))

    /// The identifier of the function.
    let identifier = CXXIdentifier(program.ast[valFunDecl].identifier?.value ?? "")

    // Determine the type of the function.
    let output: CXXTypeExpr
    if identifier.description == "main" {
      // The output type of `main` must be `int`.
      output = CXXTypeExpr("int")
    } else {
      switch program.declTypes[valFunDecl]! {
      case .lambda(let valDeclType):
        output = CXXTypeExpr(valDeclType.output, ast: program.ast)!

      case .method:
        fatalError("not implemented")

      default:
        unreachable()
      }
    }

    // Create the C++ function.
    let cxxFunDecl = cxxFunctions.count
    cxxFunctions.append(CXXFunDecl(identifier: identifier, output: output, parameters: []))

    // Update the cache and return the ID of the newly created function.
    valToCXXFunction[valFunDecl] = cxxFunDecl
    return cxxFunDecl
  }

  // MARK: Serialization

  /// Emits the C++ header of the module.
  public func emitHeader() -> String {
    var output: String = ""

    // Emit the header guard.
    output.write("#ifndef VAL_\(name.uppercased())_\n")
    output.write("#define VAL_\(name.uppercased())_\n")
    output.write("\n")

    // Emit include clauses.
    output.write("#include <variant>\n")
    output.write("\n")

    // Create a namespace for the entire module.
    output.write("namespace \(name) {\n\n")

    // Emit top-level functions.
    for decl in cxxFunctions {
      decl.writeForwardDeclaration(into: &output)
      output.write("\n")
    }

    output.write("\n}\n\n")  // module namespace
    output.write("#endif\n") // header guard

    return output
  }

  /// Emits the C++ implementation of the module.
  public func emitSource() -> String {
    var output: String = ""

    // Emit include clauses.
    output.write("#include \"\(name).h\"\n")
    output.write("\n")

    // Create a namespace for the entire module.
    output.write("namespace \(name) {\n\n")

    // Emit top-level functions.
    for decl in cxxFunctions {
      decl.writeDefinition(into: &output)
      output.write("\n")
    }

    output.write("\n}\n") // module namespace

    return output
  }

}
