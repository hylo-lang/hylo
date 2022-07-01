import Utils

/// A value that specifies the location of a declaration.
public struct DeclLocator: Hashable {

  /// A constituent of a declaration locator.
  public enum Component: Hashable {

    case conformance(target: DeclLocator, trait: String)

    case `extension`(target: DeclLocator)

    case function(name: String, labels: [String])

    case methodImpl(MethodImplDecl.Introducer)

    case module(String)

    case namespace(String)

    case lambda(NodeID<FunDecl>)

    case product(String)

    case `subscript`(name: String, labels: [String])

    case subscriptImpl(SubscriptImplDecl.Introducer)

    case trait(String)

    /// Creates a component identifying `declID` given an AST and its scope hierarchy.
    init?<T: NodeIDProtocol>(
      identifying declID: T,
      in ast: AST,
      withScopeHierarchy scopeHierarchy: ScopeHierarchy,
      withDeclTypes declTypes: DeclMap<Type>
    ) {
      switch declID.kind {
      case .conformanceDecl, .extensionDecl:
        fatalError("not implemented")

      case .funDecl:
        let declID = NodeID<FunDecl>(unsafeRawValue: declID.rawValue)

        let labels: [String]
        switch declTypes[declID]! {
        case .lambda(let type):
          labels = Array(type.labels.map({ $0 ?? "_" }))
        case .method(let type):
          labels = Array(type.inputs.map({ $0.label ?? "_" }))
        default:
          labels = []
        }

        switch ast[declID].introducer.value {
        case .memberwiseInit, .`init`:
          self = .function(name: "init", labels: labels)
        case .deinit:
          self = .function(name: "deinit", labels: [])
        case .fun:
          if let name = ast[declID].identifier?.value {
            self = .function(name: name, labels: labels)
          } else {
            self = .lambda(declID)
          }
        }

      case .methodImplDecl:
        let declID = NodeID<MethodImplDecl>(unsafeRawValue: declID.rawValue)
        self = .methodImpl(ast[declID].introducer.value)

      case .productTypeDecl:
        let declID = NodeID<ProductTypeDecl>(unsafeRawValue: declID.rawValue)
        self = .product(ast[declID].name)

      default:
        return nil
      }
    }

  }

  /// The constituents of the locator.
  public var components: [Component]

  /// Creates a locator identifying `declID` given an AST and its scope hierarchy.
  public init<T: DeclID>(
    identifying declID: T,
    in ast: AST,
    withScopeHierarchy scopeHierarchy: ScopeHierarchy,
    withDeclTypes declTypes: DeclMap<Type>
  ) {
    let last = Component(
      identifying: declID,
      in: ast,
      withScopeHierarchy: scopeHierarchy,
      withDeclTypes: declTypes)!
    components = [last]

    if let parent = scopeHierarchy.container[declID] {
      for scopeID in scopeHierarchy.scopesToRoot(from: parent) {
        if let component = Component(
          identifying: scopeID,
          in: ast,
          withScopeHierarchy: scopeHierarchy,
          withDeclTypes: declTypes)
        {
          components.append(component)
        }
      }
    }

    components.reverse()
  }

  /// The locator's value encoded as a string.
  public var mangled: String { components.descriptions(joinedBy: "") }

}

extension DeclLocator: CustomStringConvertible {

  public var description: String { mangled }

}

extension DeclLocator.Component: CustomStringConvertible {

  public var description: String {
    switch self {
    case .conformance(let target, let trait):
      return "C\(target)\(trait.prefixedByCount)"

    case .extension(let target):
      return "E\(target)"

    case .function(let name, let labels):
      let ls = labels.map({ $0.prefixedByCount }).joined()
      return "F\(name.prefixedByCount)\(labels.count)\(ls)a"

    case .methodImpl(let introducer):
      switch introducer {
      case .let  : return "Il"
      case .inout: return "Ii"
      case .sink : return "Is"
      }

    case .module(let name):
      return "M\(name.prefixedByCount)"

    case .namespace(let name):
      return "N\(name.prefixedByCount)"

    case .lambda(let discriminator):
      return "L\(discriminator.rawValue)"

    case .product(let name):
      return "P\(name.prefixedByCount)"

    case .subscript(let name, let labels):
      let ls = labels.map({ $0.prefixedByCount }).joined()
      return "S\(name.prefixedByCount)\(labels.count)\(ls)"

    case .subscriptImpl(let introducer):
      switch introducer {
      case .let   : return "Il"
      case .inout : return "Ii"
      case .sink  : return "Is"
      case .assign: return "Ia"
      }

    case .trait(let name):
      return "N\(name.prefixedByCount)"
    }
  }

}

extension String {

  var prefixedByCount: String {
    "\(count)\(self)"
  }

}
