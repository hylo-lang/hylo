import Utils

/// A type alias.
public struct TypeAliasType: TypeProtocol, Hashable {

  /// The declaration that introduces the alias.
  public let decl: NodeID<TypeAliasDecl>

  /// The name of the alias.
  public let name: Incidental<String>

  public let flags: TypeFlags = .isCanonical

  public init(decl: NodeID<TypeAliasDecl>, ast: AST) {
    self.decl = decl
    self.name = Incidental(ast[decl].name)
  }

}

extension TypeAliasType: CustomStringConvertible {

  public var description: String { name.value }

}
