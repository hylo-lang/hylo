import Utils

/// A generic value parameter.
public struct GenericValueParameterType: TypeProtocol {

  /// The declaration that introduces the parameter.
  ///
  /// - Note: The ID may denote the declaration of a generic value parameter or associated value.
  public let decl: AnyDeclID

  /// The name of the parameter.
  @Incidental public private(set) var name: String

  /// Creates an instance denoting the generic value parameter declared by `decl`.
  ///
  /// - Requires: `decl` is the ID of an associated value or generic parameter declaration.
  public init<T: DeclID>(_ decl: T, ast: AST) {
    self.decl = AnyDeclID(decl)

    switch decl.kind {
    case GenericParameterDecl.self, AssociatedValueDecl.self:
      name = (ast[decl] as! SingleEntityDecl).name
    default:
      preconditionFailure("invalid declaration")
    }
  }

  public var flags: TypeFlags { [.isCanonical, .hasGenericValueParam] }

}

extension GenericValueParameterType: CustomStringConvertible {

  public var description: String { name }

}
