import Utils

/// A generic size parameter.
public struct GenericSizeParamType: TypeProtocol, Hashable {

  /// The declaration that introduces the parameter.
  ///
  /// - Note: The ID may denote the declaration of a generic size parameter or associated size.
  public let decl: AnyDeclID

  /// The name of the parameter.
  public let name: Incidental<String>

  public let flags: TypeFlags = [.isCanonical, .hasGenericSizeParam]

  public init<T: DeclID>(decl: T, ast: AST) {
    self.decl = AnyDeclID(decl)

    switch decl.kind {
    case .genericSizeParamDecl,
         .associatedSizeDecl:
      name = Incidental((ast[decl] as! SingleEntityDecl).name)

    default:
      preconditionFailure("invalid declaration")
    }
  }

}

extension GenericSizeParamType: CustomStringConvertible {

  public var description: String { name.value }

}
