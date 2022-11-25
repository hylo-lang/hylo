import Utils

/// A generic value parameter.
public struct GenericValueParamType: TypeProtocol {

  /// The declaration that introduces the parameter.
  ///
  /// - Note: The ID may denote the declaration of a generic value parameter or associated value.
  public let decl: AnyDeclID

  /// The name of the parameter.
  public let name: Incidental<String>

  /// Creates an instance denoting the generic value parameter declared by `decl`.
  ///
  /// - Requires: `decl` is the ID of a declaration introducing an associated value or generic
  ///   value parameter.
  public init<T: DeclID>(_ decl: T, ast: AST) {
    self.decl = AnyDeclID(decl)

    switch decl.kind {
    case GenericValueParamDecl.self, AssociatedValueDecl.self:
      name = Incidental((ast[decl] as! SingleEntityDecl).name)
    default:
      preconditionFailure("invalid declaration")
    }
  }

  public var flags: TypeFlags { [.isCanonical, .hasGenericValueParam] }

}

extension GenericValueParamType: CustomStringConvertible {

  public var description: String { name.value }

}
