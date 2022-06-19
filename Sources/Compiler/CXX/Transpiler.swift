/// A Val to C++ transpiler.
public struct Transpiler {

  /// The AST of the Val sources being transpiled.
  public let ast: AST

  /// The scope hierarchy of the AST.
  public let scopeHierarchy: ScopeHierarchy

  /// The overarching type of each declaration.
  public let declTypes: DeclMap<Type>

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
  public func emitHeader(of moduleID: NodeID<ModuleDecl>) -> String {
    let module = ast[moduleID]

    // Emit the header guard.
    var header = ""
    print("#ifndef VAL_\(module.name.uppercased())", to: &header)
    print("#define VAL_\(module.name.uppercased())", to: &header)

    // Create a namespace for the entire module.
    print("\nnamespace \(module.name) {", to: &header)

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
    print("", to: &header)
    for typeDeclID in typeDeclIDs {
      print("class \(ast[typeDeclID].name);", to: &header)
    }

    // Emit type definitions.
    print("", to: &header)
    for typeDeclID in typeDeclIDs {
      header += emitTypeDefinition(of: typeDeclID)
    }

    print("\n}", to: &header) // module namespace
    print("\n#endif", to: &header) // header guard
    return header
  }

  /// Emits the specified type definition.
  private func emitTypeDefinition(of typeDeclID: NodeID<ProductTypeDecl>) -> String {
    var publicSection: [String] = []
    var privateSection: [String] = []

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
          publicSection.append(contentsOf: variables)
        } else {
          privateSection.append(contentsOf: variables)
        }

      default:
        continue
      }
    }

    // Emit the definition.
    var output = ""
    print("class \(ast[typeDeclID].name) {", to: &output)
    if !publicSection.isEmpty {
      print("public:", to: &output)
      for member in publicSection {
        print(member, to: &output)
      }
    }
    if !privateSection.isEmpty {
      print("private:", to: &output)
      for member in publicSection {
        print(member, to: &output)
      }
    }
    print("};", to: &output)
    return output
  }

  /// Emits the expression of the given type.
  public func emitTypeExpression(_ type: Type) -> String {
    switch type {
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
      fatalError("unexpected type")
    }
  }

  /// Given `type` is a nominal type, emits its fully qualified name.
  public func emitQualifiedName(_ type: Type) -> String {
    switch type {


    default:
      fatalError("unexpected type")
    }
  }

}
