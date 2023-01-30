import Utils

/// An associated type of a generic type parameter, or associated type thereof.
public struct AssociatedTypeType: TypeProtocol {

  /// The declaration that introduces the associated type in the parent trait.
  public let decl: NodeID<AssociatedTypeDecl>

  /// The domain of an associated type.
  ///
  /// The domain is either an associated type, a conformance lens, or a generic type parameter.
  public let domain: AnyType

  /// The name of the associated type.
  public let name: Incidental<String>

  /// Creates an instance denoting the associated type declared by `decl` as a member of `domain`.
  ///
  /// - Requires: `domain` is an associated type, conformance lens, or generic type parameter.
  public init(_ decl: NodeID<AssociatedTypeDecl>, domain: AnyType, ast: AST) {
    switch domain.base {
    case is AssociatedTypeType, is ConformanceLensType, is GenericTypeParameterType:
      self.domain = domain
    default:
      preconditionFailure("invalid associated type domain")
    }

    self.decl = decl
    self.name = Incidental(ast[decl].baseName)
  }

  public var flags: TypeFlags { .isCanonical }

  /// An array whose `i+1`-th element is the parent type of the `i`-th element. `components[0]` is
  /// always `self`.
  public var components: [AnyType] {
    var current = ^self
    var result = [current]

    while true {
      switch current.base {
      case is GenericTypeParameterType:
        return result

      case let type as AssociatedTypeType:
        current = type.domain
        result.append(type.domain)

      case let type as ConformanceLensType:
        current = type.subject

      default:
        unreachable()
      }
    }
  }

}

extension AssociatedTypeType: CustomStringConvertible {

  public var description: String { "\(domain).\(name.value)" }

}
