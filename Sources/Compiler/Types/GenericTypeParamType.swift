import Utils

/// A generic type parameter.
public struct GenericTypeParamType: TypeProtocol, Hashable {

  /// The declaration that introduces the parameter.
  ///
  /// - Note: The ID may denote the declaration of a generic type parameter, associated type, or
  ///   trait. In the latter case, the type denotes the trait's synthesized `Self` parameter.
  public let decl: AnyDeclID

  /// The name of the parameter.
  public let name: Incidental<String>

  public let flags: TypeFlags = [.isCanonical, .hasGenericTypeParam]

  public init<T: DeclID>(decl: T, ast: AST) {
    self.decl = AnyDeclID(decl)

    switch decl.kind {
    case .associatedTypeDecl, .genericTypeParamDecl:
      name = Incidental((ast[decl] as! SingleEntityDecl).name)
    case .traitDecl:
      name = Incidental("Self")
    default:
      preconditionFailure("invalid declaration")
    }
  }

}

extension GenericTypeParamType: CustomStringConvertible {

  public var description: String { name.value }

}
