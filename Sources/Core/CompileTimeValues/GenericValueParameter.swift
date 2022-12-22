import Utils

/// A generic compile-time value parameter.
public struct GenericValueParameter {

  /// The declaration that introduces the parameter.
  ///
  /// - Note: The ID may denote the declaration of a generic value parameter or associated value.
  public let decl: NodeID<GenericParameterDecl>

  /// The name of the parameter.
  public let name: Incidental<String>

  /// Creates an instance denoting the generic value parameter declared by `decl`.
  public init(_ decl: NodeID<GenericParameterDecl>, ast: AST) {
    self.decl = decl
    self.name = Incidental(ast[decl].name)
  }

}

extension GenericValueParameter: CustomStringConvertible {

  public var description: String { name.value }

}
