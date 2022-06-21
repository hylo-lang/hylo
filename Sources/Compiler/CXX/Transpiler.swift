import Utils

/// A Val to C++ transpiler.
public struct Transpiler {

  /// The AST of the Val sources being transpiled.
  public let ast: AST

  /// The scope hierarchy of the AST.
  public let scopeHierarchy: ScopeHierarchy

  /// The overarching type of each declaration.
  public let declTypes: DeclMap<Type>

  /// The printer used to write C++ text.
  var printer = IndentPrinter()

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
    let module = ast[moduleID]

    // Emit the header guard.
    var header = ""
    printer.write("#ifndef VAL_\(module.name.uppercased())", to: &header)
    printer.write("#define VAL_\(module.name.uppercased())", to: &header)

    // Emit include clauses.
    printer.write("", to: &header)
    printer.write("#include <utility>", to: &header)

    // Create a namespace for the entire module.
    printer.write("", to: &header)
    printer.write("namespace \(module.name) {", to: &header)

    // Collect public type declarations.
    var typeDeclIDs: [NodeID<ProductTypeDecl>] = []
    for memberID in module.members {
      switch memberID.kind {
      case .productTypeDecl:
        let typeDeclID = NodeID<ProductTypeDecl>(converting: memberID)!
        if !ast[typeDeclID].isPublic { continue }
        typeDeclIDs.append(typeDeclID)

      default:
        continue
      }
    }

    // Emit type declarations.
    printer.write("", to: &header)
    for typeDeclID in typeDeclIDs {
      printer.write("class \(ast[typeDeclID].name);", to: &header)
    }

    // Emit type definitions.
    printer.write("", to: &header)
    for typeDeclID in typeDeclIDs {
      header += emitTypeDefinition(of: typeDeclID)
    }

    printer.write("\n}", to: &header) // module namespace
    printer.write("\n#endif", to: &header) // header guard
    return header
  }

  /// Emits the specified type definition.
  private mutating func emitTypeDefinition(of typeDeclID: NodeID<ProductTypeDecl>) -> String {
    var cxxDecl = CXXTypeDecl(name: ast[typeDeclID].name)

    for memberID in ast[typeDeclID].members {
      switch memberID.kind {
      case .bindingDecl:
        let bindingDeclID = NodeID<BindingDecl>(converting: memberID)!
        let bindingDecl = ast[bindingDeclID]

        let variables = bindingDecl.pattern.names(ast: ast).map({ (name) -> String in
          var definition = ast[bindingDeclID].isStatic
            ? "static "
            : ""

          let varDeclID = ast[name].decl
          definition += emitTypeExpression(declTypes[varDeclID]!)
          definition += " "
          definition += ast[varDeclID].name
          definition += ";"
          return definition
        })

        if bindingDecl.isPublic {
          cxxDecl.publicFields.append(contentsOf: variables)
        } else {
          cxxDecl.privateFields.append(contentsOf: variables)
        }

      case .funDecl:
        let methodDeclID = NodeID<FunDecl>(converting: memberID)!
        let methodDecl = ast[methodDeclID]
        assert(!methodDecl.isStatic, "not implemented")

        switch methodDecl.introducer.value {
        case .memberwiseInit:
          let definition = emitMemberwiseInit(typeDeclID: typeDeclID, initDeclID: methodDeclID)
          if methodDecl.isPublic {
            cxxDecl.publicCtors.append(definition)
          } else {
            cxxDecl.privateCtors.append(definition)
          }

        default:
          fatalError("not implemented")
        }

      default:
        continue
      }
    }

    var output = ""
    cxxDecl.write(into: &output, using: &printer)
    return output
  }

  /// Emits the specified memberwise initializer definition.
  private func emitMemberwiseInit(
    typeDeclID: NodeID<ProductTypeDecl>,
    initDeclID: NodeID<FunDecl>
  ) -> String {
    // Note: the first parameter of the signature is the receiver.
    guard case .lambda(let initDeclType) = declTypes[initDeclID] else { unreachable() }
    var definition = "\(ast[typeDeclID].name)"

    // Emit the parameters.
    definition += "("
    definition += initDeclType.inputs
      .dropFirst()
      .map({ (p) -> String in "\(emitTypeExpression(p.type)) \(p.label!)" })
      .joined(separator: ", ")
    definition += ")"

    // Emit the memberwise initialization.
    if !initDeclType.inputs.isEmpty {
      definition += ": "
      definition += initDeclType.inputs
        .dropFirst()
        .map({ (p) -> String in "\(p.label!)(std::move(\(p.label!)))" })
        .joined(separator: ", ")
    }

    definition += " {}"
    return definition
  }

  /// Emits the expression of the given type.
  private func emitTypeExpression(_ type: Type) -> String {
    switch type {
    case .parameter(let type):
      switch type.convention {
      case .let:
        return emitTypeExpression(type.bareType) + "const&"
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
