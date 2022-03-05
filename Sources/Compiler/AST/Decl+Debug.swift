extension Decl {

  /// A debug identifier for the declaration.
  public var debugID: String {
    var components: [String] = []
    var next: Decl? = self

    outer:while let node = next {
      switch node {
      case let delc as ModuleDecl:
        components.append(delc.ident)

      case let decl as VarDecl:
        components.append(decl.ident)

      case let decl as FunParamDecl:
        components.append(decl.ident)

      case let decl as BaseFunDecl:
        let sign = decl.params.map({ ($0.label ?? "_") + ":" }).joined()
        let name = decl.ident.isEmpty ? "$\(decl.discriminator)" : decl.ident
        components.append("\(name)(\(sign))")

      case let decl as NominalTypeDecl:
        components.append(decl.ident)

      case let decl as AliasTypeDecl:
        components.append(decl.ident)

      case is ExtensionDecl:
        break

      case let decl as NamespaceDecl:
        components.append(decl.ident)

      default:
        let id = String(Int(bitPattern: ObjectIdentifier(node)), radix: 36)
        components.append("\(type(of: node))@\(id)")
      }

      var parent = node.parentDeclSpace
      while parent != nil {
        if let decl = parent! as? Decl {
          next = decl
          continue outer
        } else {
          parent = parent!.parentDeclSpace
        }
      }
      break
    }

    return components.reversed().joined(separator: "::")
  }

}
