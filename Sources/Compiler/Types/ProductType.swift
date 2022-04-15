import Utils

/// A nominal product type.
public struct ProductType: TypeProtocol, Hashable {

  /// The declaration that introduces the alias.
  public let decl: NodeID<ProductTypeDecl>

  /// The name of the product type.
  public let name: Incidental<String>

  public let flags: TypeFlags = .isCanonical

  public init(decl: NodeID<ProductTypeDecl>, ast: AST) {
    self.decl = decl
    self.name = Incidental(ast[decl].name)
  }

}

extension ProductType: CustomStringConvertible {

  public var description: String { name.value }

}
