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
  public private(set) var cxxFunctions: [CXXFunctionDecl] = []

  /// The C++ function bodied for this module.
  /// The size of this array is equal to the size of `cxxFunctions` array
  public private(set) var cxxFunctionBodies: [CXXRepresentable?] = []

  /// A table mapping val function declarations to the ID of the corresponding C++ declaration.
  private var valToCXXFunction: [FunctionDecl.Typed: Int] = [:]

  /// The C++ classes declared in this module.
  private var cxxClasses: [CXXClassDecl] = []

  /// A table mapping val type declarations to the ID of the corresponding C++ class declaration.
  private var valToCXXClass: [ProductTypeDecl.Typed: Int] = [:]

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
        parameters: cxxParams,
        original: valFunctionDecl))
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

  public mutating func getOrCreateClass(
    correspondingTo decl: ProductTypeDecl.Typed
  ) -> CXXClassDecl.ID {
    if let cxxClassDecl = valToCXXClass[decl] { return cxxClassDecl }

    assert(program.isGlobal(decl.id))

    let name = CXXIdentifier(decl.identifier.value)

    // Transpile the class membmers.
    var cxxMembers: [CXXClassDecl.ClassMember] = []
    for member in decl.members {
      switch member.kind {
      case BindingDecl.self:
        let bindingDecl = BindingDecl.Typed(member)!
        // Check if the attribute is static or not.
        var isStatic = false
        if bindingDecl.memberModifier != nil {
          switch bindingDecl.memberModifier!.value {
          case .static:
            isStatic = true
          }
        }
        // TODO: visit initializer (bindingDecl.initializer)
        let cxxInitializer: CXXRepresentable? = nil
        // TODO: pattern introducer (let, var, sink, inout)
        // Visit the name patterns.
        for (_, name) in bindingDecl.pattern.subpattern.names {
          let varDecl = name.decl
          let cxxAttribute = CXXClassAttribute(
            type: CXXTypeExpr(varDecl.type, ast: program.ast)!,
            name: CXXIdentifier(varDecl.name),
            initializer: cxxInitializer,
            isStatic: isStatic,
            original: varDecl)
          cxxMembers.append(.attribute(cxxAttribute))
        }

      case InitializerDecl.self:
        let initialzerDecl = InitializerDecl.Typed(member)!
        switch initialzerDecl.introducer.value {
        case .`init`:
          // TODO: emit constructor
          cxxMembers.append(.constructor)
          break
        case .memberwiseInit:
          // TODO: emit constructor
          cxxMembers.append(.constructor)
          break
        }

      case MethodDecl.self:
        cxxMembers.append(.method)

      default:
        unreachable("unexpected class member")
      }
    }

    // Create the C++ class.
    let cxxClassDeclID = cxxClasses.count
    cxxClasses.append(CXXClassDecl(name: name, members: cxxMembers, original: decl))

    // Update the cache and return the ID of the newly created class.
    valToCXXClass[decl] = cxxClassDeclID
    return cxxClassDeclID
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
