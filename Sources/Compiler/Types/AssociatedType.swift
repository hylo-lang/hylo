import Utils

/// A type that refers to a type member of an existentially quantified generic type parameter.
public struct AssociatedType: TypeProtocol, Hashable {

  /// The declaration that introduces the associated type in the parent trait.
  public let decl: NodeID<AssociatedTypeDecl>

  /// The domain of an associated type.
  ///
  /// - Note: The domain can be either a generic type parameter or another associated type.
  public let domain: Type

  /// The name of the associated type.
  public let name: Incidental<String>

  public let flags: TypeFlags = [.isCanonical]

  public init(decl: NodeID<AssociatedTypeDecl>, domain: Type, ast: AST) {
    self.decl = decl
    self.name = Incidental(ast[decl].name)

    switch domain {
    case .associated, .genericTypeParam:
      self.domain = domain
    default:
      preconditionFailure("invalid associated type domain")
    }
  }

}

extension AssociatedType: CustomStringConvertible {

  public var description: String { "\(domain).\(name.value)" }

}
