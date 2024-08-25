import Utils

/// The type of an associated value of a generic type parameter, or associated type thereof.
public struct AssociatedValueType: TypeProtocol {

  /// The declaration that introduces the associated value in the parent trait.
  public let decl: AssociatedValueDecl.ID

  /// The domain of an associated value.
  ///
  /// The domain is either an associated type, a conformance lens, or a generic type parameter.
  public let domain: AnyType

  /// The name of the associated type.
  public let name: Incidental<String>

  /// Creates an instance denoting the associated value declared by `decl` as a member of `domain`.
  ///
  /// - Requires: `domain` is an associated type, conformance lens, or generic type parameter.
  public init(_ decl: AssociatedValueDecl.ID, domain: AnyType, ast: AST) {
    switch domain.base {
    case is AssociatedTypeType, is ConformanceLensType, is GenericTypeParameterType:
      self.domain = domain
    default:
      preconditionFailure("invalid associated value domain")
    }

    self.decl = decl
    self.name = Incidental(ast[decl].baseName)
  }

  public var flags: ValueFlags { .init() }

}

extension AssociatedValueType: CustomStringConvertible {

  public var description: String { "\(domain).\(name.value)" }

}
