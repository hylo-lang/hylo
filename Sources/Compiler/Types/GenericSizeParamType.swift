import Utils

/// A generic value parameter.
public struct GenericValueParamType: TypeProtocol, Hashable {

  /// The declaration that introduces the parameter.
  ///
  /// - Note: The ID may denote the declaration of a generic value parameter or associated value.
  public let decl: AnyDeclID

  /// The name of the parameter.
  public let name: Incidental<String>

  public let flags: TypeFlags = [.isCanonical, .hasGenericValueParam]

  public init<T: DeclID>(decl: T, ast: AST) {
    self.decl = AnyDeclID(decl)

    switch decl.kind {
    case GenericValueParamDecl.self,
         AssociatedValueDecl.self:
      name = Incidental((ast[decl] as! SingleEntityDecl).name)

    default:
      preconditionFailure("invalid declaration")
    }
  }

}

extension GenericValueParamType: CustomStringConvertible {

  public var description: String { name.value }

}
