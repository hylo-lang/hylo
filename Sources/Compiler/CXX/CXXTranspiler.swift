import Utils

/// A Val to C++ transpiler.
public struct CXXTranspiler {

  /// The program being transpiled.
  public let program: TypedProgram

  /// The type definitions explosed in the C++ API.
  private var publicTypeDefinitions: [NodeID<ProductTypeDecl>: CXXTypeDecl] = [:]

  /// Creates a C++ transpiler with a well-typed AST.
  public init(program: TypedProgram) {
    self.program = program
  }

  // MARK: API

  /// Emits that C++ module correspondng to the Val module
  ///
  /// - Parameters:
  ///   - moduleID: The ID of the module declaration whose header is generated.
  public mutating func emitModule(of moduleID: NodeID<ModuleDecl>) -> CXXModuleDecl {
    let moduleDecl = program.ast[moduleID]
    return CXXModuleDecl(original: moduleDecl)
  }

  /// Emits a C++ header file of the specified module.
  ///
  /// - Parameters:
  ///   - moduleID: The ID of the module declaration whose header is generated.
  public mutating func emitHeader(of moduleID: NodeID<ModuleDecl>) -> String {
    let moduleDecl = program.ast[moduleID]

    // Reinitialize the state of the transpiler.
    publicTypeDefinitions.removeAll()

    // Emit the header guard.
    var header = ""
    header.write("#ifndef VAL_\(moduleDecl.name.uppercased())\n")
    header.write("#define VAL_\(moduleDecl.name.uppercased())\n")

    // Emit include clauses.
    header.write("#include <utility>\n")
    header.write("#include <functional>\n")

    // Create a namespace for the entire module.
    header.write("namespace \(moduleDecl.name) {\n")

    // Collect public declarations from Val.
    for memberID in program.ast.topLevelDecls(moduleID) {
      switch memberID.kind {
      case .productTypeDecl:
        let typeDeclID = NodeID<ProductTypeDecl>(converting: memberID)!
        if program.ast[typeDeclID].isPublic && (publicTypeDefinitions[typeDeclID] == nil) {
          publicTypeDefinitions[typeDeclID] = CXXTypeDecl(name: program.ast[typeDeclID].name)
        }

      case .conformanceDecl, .extensionDecl, .funDecl, .namespaceDecl, .subscriptDecl:
        // just ignore them for now
        continue
        // fatalError("not implemented")

      default:
        continue
      }
    }

    // Emit type declarations.
    for typeDeclID in publicTypeDefinitions.keys {
      header.write("class \(program.ast[typeDeclID].name);\n")
    }

    // Emit type definitions.
    for typeDeclID in publicTypeDefinitions.keys {
      header += emitTypeDeclAPI(of: typeDeclID)
    }

    header.write("}\n") // module namespace
    header.write("#endif\n") // header guard
    return header
  }

  /// Emits the definition of the specified type declaration.
  private mutating func emitTypeDeclAPI(of typeDeclID: NodeID<ProductTypeDecl>) -> String {
    var cxxDecl = CXXTypeDecl(name: program.ast[typeDeclID].name)

    for memberID in program.ast[typeDeclID].members {
      switch memberID.kind {
      case .bindingDecl:
        let bindingDeclID = NodeID<BindingDecl>(converting: memberID)!
        let bindingDecl = program.ast[bindingDeclID]

        // Stored properties are always emitted.
        let access: CXXTypeDecl.SectionAccess = bindingDecl.isPublic
          ? .public
          : .private
        cxxDecl.fields.append(contentsOf: program.ast.names(in: bindingDecl.pattern)
          .map({ (_, name) -> (String, CXXTypeDecl.SectionAccess) in
            var definition = program.ast[bindingDeclID].isStatic
              ? "static "
              : ""

            let varDeclID = program.ast[name].decl
            definition += emitTypeExpr(program.declTypes[varDeclID]!)
            definition += " "
            definition += program.ast[varDeclID].name
            definition += ";"
            return (definition, access)
          }))

      case .funDecl:
        let methodDeclID = NodeID<FunDecl>(converting: memberID)!
        let methodDecl = program.ast[methodDeclID]

        // Memberwise constructors are always emitted.
        if methodDecl.introducer.value == .memberwiseInit {
          let definition = emitMemberwiseInit(typeDeclID: typeDeclID, initDeclID: methodDeclID)
          if methodDecl.isPublic {
            cxxDecl.publicCtors.append(definition)
          } else {
            cxxDecl.privateCtors.append(definition)
          }
          continue
        }

        // Other constructors and methods are not emitted unless they are public.
        if !methodDecl.isPublic { continue }
        assert(!methodDecl.isStatic, "not implemented")

        switch methodDecl.introducer.value {
        case .memberwiseInit:
          unreachable()
        case .fun:
          cxxDecl.methods.append(contentsOf: emitPublicMethodDecl(methodDeclID))
        default:
          fatalError("not implemented")
        }

      default:
        continue
      }
    }

    var output = ""
    cxxDecl.write(into: &output)
    return output
  }

  /// Emits the definition of the specified memberwise initializer.
  private func emitMemberwiseInit(
    typeDeclID: NodeID<ProductTypeDecl>,
    initDeclID: NodeID<FunDecl>
  ) -> String {
    // Note: the first parameter of the signature is the receiver.
    let initType = LambdaType(converting: program.declTypes[initDeclID]!)!
    var definition = "explicit \(program.ast[typeDeclID].name)"

    // Emit the parameters.
    definition += "("
    definition += initType.inputs
      .dropFirst()
      .map({ (p) -> String in "\(emitTypeExpr(p.type)) \(p.label!)" })
      .joined(separator: ", ")
    definition += ")"

    // Emit the memberwise initialization.
    if !initType.inputs.isEmpty {
      definition += ": "
      definition += initType.inputs
        .dropFirst()
        .map({ (p) -> String in "\(p.label!)(std::move(\(p.label!)))" })
        .joined(separator: ", ")
    }

    definition += " {}"
    return definition
  }

  /// Emits the declarations(s) of the specified method declaration.
  ///
  /// - Note: The method returns multiple definitions if `methodDeclID` denotes a method bundle.
  private func emitPublicMethodDecl(_ methodDeclID: NodeID<FunDecl>) -> [String] {
    let methodName = program.ast[methodDeclID].identifier!.value

    switch program.declTypes[methodDeclID] {
    case .lambda(let type):
      var signature = emitTypeExpr(type.output)
      signature += " \(methodName)("
      signature += type.inputs
        .map({ p in emitTypeExpr(p.type) })
        .joined(separator: ", ")
      signature += ")"

      if program.ast[methodDeclID].isSink {
        signature += " &&"
      } else if !program.ast[methodDeclID].isSink {
        signature += " const"
      }

      signature += ";"
      return [signature]

    case .method(let type):
      // The input/output is the same for all variants.
      let output = emitTypeExpr(type.output)
      let inputs = type.inputs
        .map({ p in emitTypeExpr(p.type) })
        .joined(separator: ", ")

      return type.capabilities.map({ (capability) -> String in
        switch capability {
        case .let:
          return "\(output) \(methodName)(\(inputs)) const;"
        case .inout:
          return "void inplace_\(methodName)(\(inputs));"
        case .sink:
          return "\(output) sink_\(methodName)(\(inputs)) &&;"
        }
      })

    default:
      unreachable()
    }
  }

  /// Emits the C++ expression of the given type.
  private func emitTypeExpr(_ type: Type) -> String {
    switch type {
    case .parameter(let type):
      switch type.convention {
      case .let:
        return emitTypeExpr(type.bareType) + " const&"
      case .inout:
        return emitTypeExpr(type.bareType) + "&"
      case .sink:
        return emitTypeExpr(type.bareType) + "&&"
      default:
        unreachable("unexpected parameter passing convention")
      }

    case .product(let type):
      var nameComponents: [String] = []
      for scopeID in program.scopeHierarchy.scopesToRoot(from: type.decl) {
        switch scopeID.kind {
        case .moduleDecl:
          nameComponents.append(program.ast[NodeID<ModuleDecl>(rawValue: scopeID.rawValue)].name)

        default:
          if let decl = program.ast[scopeID] as? SingleEntityDecl {
            nameComponents.append(decl.name)
          }
        }

      }
      return nameComponents.reversed().joined(separator: "::")

    default:
      unreachable("unexpected type")
    }
  }

}
