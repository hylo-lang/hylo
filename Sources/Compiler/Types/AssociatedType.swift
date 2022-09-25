import Utils

/// An associated type of a generic type parameter, or associated type thereof.
public struct AssociatedType: TypeProtocol, Hashable {

  /// The declaration that introduces the associated type in the parent trait.
  public let decl: NodeID<AssociatedTypeDecl>

  /// The domain of an associated type.
  ///
  /// The domain is either an associated type, a conformance lens, or a generic type parameter.
  public let domain: Type

  /// The name of the associated type.
  public let name: Incidental<String>

  public let flags: TypeFlags = .isCanonical

  public init(decl: NodeID<AssociatedTypeDecl>, domain: Type, ast: AST) {
    switch domain {
    case .associatedType, .conformanceLens, .genericTypeParam:
      self.domain = domain
    default:
      preconditionFailure("invalid associated type domain")
    }

    self.decl = decl
    self.name = Incidental(ast[decl].name)
  }

}

extension AssociatedType: CustomStringConvertible {

  public var description: String { "\(domain).\(name.value)" }

}
