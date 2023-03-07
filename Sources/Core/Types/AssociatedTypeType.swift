import Utils

/// An associated type of a generic type parameter, or associated type thereof.
public struct AssociatedTypeType: TypeProtocol {

  /// The declaration that introduces the associated type in the parent trait.
  public let decl: AssociatedTypeDecl.ID

  /// The type whose `self` is member.
  ///
  /// `domain` is either an associated type, a conformance lens, or a generic type parameter.
  public let domain: AnyType

  /// The name of the associated type.
  public let name: Incidental<String>

  /// Creates an instance denoting the associated type declared by `decl` as a member of `domain`.
  ///
  /// - Requires: `domain` is an associated type, conformance lens, or generic type parameter.
  public init(_ decl: AssociatedTypeDecl.ID, domain: AnyType, ast: AST) {
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

}

extension AssociatedTypeType: CustomStringConvertible {

  public var description: String { "\(domain).\(name.value)" }

}
