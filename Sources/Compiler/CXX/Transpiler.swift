import Utils

/// A Val to C++ transpiler.
public struct Transpiler {

  /// The AST of the Val sources being transpiled.
  public let ast: AST

  /// The scope hierarchy of the AST.
  public let scopeHierarchy: ScopeHierarchy

  /// The overarching type of each declaration.
  public let declTypes: DeclMap<Type>

  /// The type definitions explosed in the C++ API.
  private var publicTypeDefinitions: [NodeID<ProductTypeDecl>: CXXTypeDecl] = [:]

  /// Creates a C++ transpiler with a well-typed AST.
  public init(ast: AST, scopeHierarchy: ScopeHierarchy, declTypes: DeclMap<Type>) {
    self.ast = ast
    self.declTypes = declTypes
    self.scopeHierarchy = scopeHierarchy
  }

  /// Emits a C++ header file of the specified module.
  ///
  /// - Parameters:
  ///   - moduleID: The ID of the module declaration whose header is generated.
  public mutating func emitHeader(of moduleID: NodeID<ModuleDecl>) -> String {
    let moduleDecl = ast[moduleID]

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
    for memberID in moduleDecl.members {
      switch memberID.kind {
      case .productTypeDecl:
        let typeDeclID = NodeID<ProductTypeDecl>(converting: memberID)!
        if ast[typeDeclID].isPublic && (publicTypeDefinitions[typeDeclID] == nil) {
          publicTypeDefinitions[typeDeclID] = CXXTypeDecl(name: ast[typeDeclID].name)
        }

      case .conformanceDecl, .extensionDecl, .funDecl, .namespaceDecl, .subscriptDecl:
        fatalError("not implemented")

      default:
        continue
      }
    }

    // Emit type declarations.
    for typeDeclID in publicTypeDefinitions.keys {
      header.write("class \(ast[typeDeclID].name);\n")
    }

    // Emit type definitions.
    for typeDeclID in publicTypeDefinitions.keys {
      header += emitTypeDefinition(of: typeDeclID)
    }

    header.write("}\n") // module namespace
    header.write("#endif\n") // header guard
    return header
  }

  /// Emits the definition of the specified type declaration.
  private mutating func emitTypeDefinition(of typeDeclID: NodeID<ProductTypeDecl>) -> String {
    var cxxDecl = CXXTypeDecl(name: ast[typeDeclID].name)

    for memberID in ast[typeDeclID].members {
      switch memberID.kind {
      case .bindingDecl:
        let bindingDeclID = NodeID<BindingDecl>(converting: memberID)!
        let bindingDecl = ast[bindingDeclID]

        // Stored properties are always emitted.
        let access: CXXTypeDecl.SectionAccess = bindingDecl.isPublic
          ? .public
          : .private
        cxxDecl.fields.append(contentsOf: ast.names(in: bindingDecl.pattern)
          .map({ (_, name) -> (String, CXXTypeDecl.SectionAccess) in
            var definition = ast[bindingDeclID].isStatic
              ? "static "
              : ""

            let varDeclID = ast[name].decl
            definition += emitTypeExpression(declTypes[varDeclID]!)
            definition += " "
            definition += ast[varDeclID].name
            definition += ";"
            return (definition, access)
          }))

      case .funDecl:
        let methodDeclID = NodeID<FunDecl>(converting: memberID)!
        let methodDecl = ast[methodDeclID]

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
          cxxDecl.methods.append(contentsOf: emitMethod(methodDeclID))
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
    guard case .lambda(let initType) = declTypes[initDeclID] else { unreachable() }
    var definition = "explicit \(ast[typeDeclID].name)"

    // Emit the parameters.
    definition += "("
    definition += initType.inputs
      .dropFirst()
      .map({ (p) -> String in "\(emitTypeExpression(p.type)) \(p.label!)" })
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

  /// Emits the definition(s) of the specified method declaration.
  ///
  /// - Note: The method returns multiple definitions if `methodDeclID` denotes a method bundle.
  private func emitMethod(_ methodDeclID: NodeID<FunDecl>) -> [String] {
    let methodName = ast[methodDeclID].identifier!.value

    switch declTypes[methodDeclID] {
    case .lambda(let type):
      var signature = emitTypeExpression(type.output)
      signature += " \(methodName)("
      signature += type.inputs
        .map({ p in emitTypeExpression(p.type) })
        .joined(separator: ", ")
      signature += ")"

      if ast[methodDeclID].isSink {
        signature += " &&"
      } else if !ast[methodDeclID].isSink {
        signature += " const"
      }

      signature += ";"
      return [signature]

    case .method(let type):
      // The input/output is the same for all variants.
      let output = emitTypeExpression(type.output)
      let inputs = type.inputs
        .map({ p in emitTypeExpression(p.type) })
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

  /// Emits the expression of the given type.
  private func emitTypeExpression(_ type: Type) -> String {
    switch type {
    case .parameter(let type):
      switch type.convention {
      case .let:
        return emitTypeExpression(type.bareType) + " const&"
      case .inout:
        return emitTypeExpression(type.bareType) + "&"
      case .sink:
        return emitTypeExpression(type.bareType) + "&&"
      default:
        unreachable("unexpected parameter passing convention")
      }

    case .product(let type):
      var nameComponents: [String] = []
      for scopeID in scopeHierarchy.scopesToRoot(from: type.decl) {
        switch scopeID.kind {
        case .moduleDecl:
          nameComponents.append(ast[NodeID<ModuleDecl>(unsafeRawValue: scopeID.rawValue)].name)

        default:
          if let decl = ast[scopeID] as? SingleEntityDecl {
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
