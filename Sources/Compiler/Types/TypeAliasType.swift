import Utils

/// A type alias.
public struct TypeAliasType: TypeProtocol {

  /// The declaration that introduces the alias.
  public let decl: NodeID<TypeAliasDecl>

  /// The name of the alias.
  public let name: Incidental<String>

  /// Creates an instance denoting the product type declared by `decl`.
  public init(_ decl: NodeID<TypeAliasDecl>, ast: AST) {
    self.decl = decl
    self.name = Incidental(ast[decl].name)
  }

  public var flags: TypeFlags { .isCanonical }

}

extension TypeAliasType: CustomStringConvertible {

  public var description: String { name.value }

}
