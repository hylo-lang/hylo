import Utils

/// A generic type parameter.
public struct GenericTypeParamType: TypeProtocol {

  /// The declaration that introduces the parameter.
  ///
  /// - Note: The ID may denote the declaration of a generic type parameter, associated type, or
  ///   trait. In the latter case, the type denotes the trait's synthesized `Self` parameter.
  public let decl: AnyDeclID

  /// The name of the parameter.
  public let name: Incidental<String>

  /// Creates an instance denoting the generic type parameter declared by `decl`.
  ///
  /// - Requires: `decl` is the ID of a declaration introducing an associated type, generic type
  ///   parameter, or trait.
  public init<T: DeclID>(_ decl: T, ast: AST) {
    self.decl = AnyDeclID(decl)

    switch decl.kind {
    case AssociatedTypeDecl.self, GenericTypeParamDecl.self:
      name = Incidental((ast[decl] as! SingleEntityDecl).name)
    case TraitDecl.self:
      name = Incidental("Self")
    default:
      preconditionFailure("invalid declaration")
    }
  }

  public var flags: TypeFlags { [.isCanonical, .hasGenericTypeParam] }

}

extension GenericTypeParamType: CustomStringConvertible {

  public var description: String { name.value }

}
