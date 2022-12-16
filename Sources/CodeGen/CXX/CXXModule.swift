import Utils
import FrontEnd
import Core

/// A C++ module.
public struct CXXModule {

  /// The module's declaration in Val's AST.
  public let valDecl: ModuleDecl.Typed

  /// The typed program for wich we are constructing the CXX translation.
  public let program: TypedProgram

  /// The C++ functions declared in `self`.
  public private(set) var cxxFunctions: [CXXFunctionDecl] = []

  /// The C++ function bodied for this module.
  /// The size of this array is equal to the size of `cxxFunctions` array
  public private(set) var cxxFunctionBodies: [CXXRepresentable?] = []

  /// A table mapping val function declarations to the ID of the corresponding C++ declaration.
  private var valToCXXFunction: [FunctionDecl.Typed: Int] = [:]

  public init(_ decl: ModuleDecl.Typed, for program: TypedProgram) {
    self.valDecl = decl
    self.program = program
  }

  /// Returns the ID of the C++ function declaration corresponding to `valFunctionDecl`.
  ///
  /// - Requires: `valFunctionDecl` must be declared in `self.decl`.
  public mutating func getOrCreateFunction(
    correspondingTo valFunctionDecl: FunctionDecl.Typed
  ) -> CXXFunctionDecl.ID {
    if let cxxFunctionDecl = valToCXXFunction[valFunctionDecl] { return cxxFunctionDecl }

    assert(program.isGlobal(valFunctionDecl.id))

    /// The identifier of the function.
    let identifier = CXXIdentifier(valFunctionDecl.identifier?.value ?? "")

    // Determine the output type of the function.
    let output: CXXTypeExpr
    if identifier.description == "main" {
      // The output type of `main` must be `int`.
      output = CXXTypeExpr("int")
    } else {
      switch valFunctionDecl.type.base {
      case let valDeclType as LambdaType:
        output = CXXTypeExpr(valDeclType.output, ast: program.ast, asReturnType: true)!

      case is MethodType:
        fatalError("not implemented")

      default:
        unreachable()
      }
    }

    // Determine the parameter types of the function.
    let paramTypes: [CallableTypeParameter]
    switch valFunctionDecl.type.base {
    case let valDeclType as LambdaType:
      paramTypes = valDeclType.inputs

    case is MethodType:
      fatalError("not implemented")

    default:
      unreachable()
    }

    // Determine the parameters of the function.
    assert(paramTypes.count == valFunctionDecl.parameters.count)
    var cxxParams: [CXXFunctionDecl.Parameter] = []
    for (i, param) in valFunctionDecl.parameters.enumerated() {
      let name = CXXIdentifier(param.name)
      let type = CXXTypeExpr(paramTypes[i].type, ast: program.ast)
      cxxParams.append(CXXFunctionDecl.Parameter(name, type!))
    }

    // Create the C++ function.
    let cxxFunctionDecl = cxxFunctions.count
    cxxFunctions.append(
      CXXFunctionDecl(
        identifier: identifier,
        output: output,
        parameters: cxxParams))
    // Associate an empty body to it.
    cxxFunctionBodies.append(nil)

    // Update the cache and return the ID of the newly created function.
    valToCXXFunction[valFunctionDecl] = cxxFunctionDecl
    return cxxFunctionDecl
  }

  /// Set the body for the function with the given ID.
  public mutating func setFunctionBody(
    _ body: CXXRepresentable?,
    forID cxxFunID: CXXFunctionDecl.ID
  ) {
    cxxFunctionBodies[cxxFunID] = body
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

    // Emit top-level functions.
    for (i, decl) in cxxFunctions.enumerated() {
      if let body = cxxFunctionBodies[i] {
        decl.writeSignature(into: &output)
        output.write(" ")
        body.writeCode(into: &output)
      }
    }

    output.write("\n}\n")  // module namespace

    return output
  }

}
