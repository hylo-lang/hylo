import Utils

/// A nominal product type.
public struct ProductType: TypeProtocol {

  /// The declaration that introduces the alias.
  public let decl: NodeID<ProductTypeDecl>

  /// The name of the product type.
  public let name: Incidental<String>

  /// Creates an instance denoting the product type declared by `decl`.
  public init(_ decl: NodeID<ProductTypeDecl>, ast: AST) {
    self.decl = decl
    self.name = Incidental(ast[decl].baseName)
  }

  public var flags: TypeFlags { .isCanonical }

}

extension ProductType: CustomStringConvertible {

  public var description: String { name.value }

}
