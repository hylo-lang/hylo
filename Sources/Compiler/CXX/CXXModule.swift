import Utils

/// A C++ module.
public struct CXXModule {

  /// The module's declaration in Val's AST.
  public let valDecl: NodeID<ModuleDecl>

  /// The module's name.
  public let name: String

  /// The C++ functions declared in `self`.
  public private(set) var cxxFunctions: [CXXFunctionDecl] = []

  /// A table mapping val function declarations to the ID of the corresponding C++ declaration.
  private var valToCXXFunction: [NodeID<FunctionDecl>: Int] = [:]

  public init(valDecl: NodeID<ModuleDecl>, name: String) {
    self.valDecl = valDecl
    self.name = name
  }

  /// Returns the ID of the C++ function declaration corresponding to `valFunctionDecl`.
  ///
  /// - Requires: `valFunctionDecl` must be declared in `self.decl`.
  public mutating func getOrCreateFunction(
    correspondingTo valFunctionDecl: NodeID<FunctionDecl>,
    program: TypedProgram
  ) -> CXXFunctionDecl.ID {
    if let cxxFunctionDecl = valToCXXFunction[valFunctionDecl] { return cxxFunctionDecl }

    assert(program.isGlobal(valFunctionDecl))

    /// The identifier of the function.
    let identifier = CXXIdentifier(program.ast[valFunctionDecl].identifier?.value ?? "")

    // Determine the output type of the function.
    let output: CXXTypeExpr
    if identifier.description == "main" {
      // The output type of `main` must be `int`.
      output = CXXTypeExpr("int")
    } else {
      switch program.declTypes[valFunctionDecl]! {
      case .lambda(let valDeclType):
        output = CXXTypeExpr(valDeclType.output, ast: program.ast, asReturnType: true)!

      case .method:
        fatalError("not implemented")

      default:
        unreachable()
      }
    }

    // Determine the parameter types of the function.
    let paramTypes: [CallableTypeParameter]
    switch program.declTypes[valFunctionDecl]! {
    case .lambda(let valDeclType):
      paramTypes = valDeclType.inputs

    case .method:
      fatalError("not implemented")

    default:
      unreachable()
    }

    // Determine the parameters of the function.
    assert(paramTypes.count == program.ast[valFunctionDecl].parameters.count)
    var cxxParams: [CXXFunctionDecl.Parameter] = []
    for (i, paramID) in program.ast[valFunctionDecl].parameters.enumerated() {
      let name = CXXIdentifier(program.ast[paramID].name)
      let type = CXXTypeExpr(paramTypes[i].type, ast: program.ast)
      cxxParams.append(CXXFunctionDecl.Parameter(name, type!))
    }

    // Create the C++ function.
    let cxxFunctionDecl = cxxFunctions.count
    cxxFunctions.append(CXXFunctionDecl(
      identifier: identifier,
      output: output,
      parameters: cxxParams))

    // Update the cache and return the ID of the newly created function.
    valToCXXFunction[valFunctionDecl] = cxxFunctionDecl
    return cxxFunctionDecl
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
