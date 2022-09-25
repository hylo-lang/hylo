import Utils

/// The type of an associated value of a generic type parameter, or associated type thereof.
public struct AssociatedValue: TypeProtocol, Hashable {

  /// The declaration that introduces the associated value in the parent trait.
  public let decl: NodeID<AssociatedValueDecl>

  /// The domain of an associated value.
  ///
  /// The domain is either an associated type, a conformance lens, or a generic type parameter.
  public let domain: Type

  /// The name of the associated type.
  public let name: Incidental<String>

  public let flags: TypeFlags = .isCanonical

  public init(decl: NodeID<AssociatedValueDecl>, domain: Type, ast: AST) {
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

extension AssociatedValue: CustomStringConvertible {

  public var description: String { "\(domain).\(name.value)" }

}
