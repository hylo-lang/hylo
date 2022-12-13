import Utils

/// A nominal product type.
public struct ProductType: TypeProtocol {

  /// The declaration that introduces the alias.
  public let decl: NodeID<ProductTypeDecl>

  /// The name of the product type.
  @Incidental public private(set) var name: String

  /// Creates an instance denoting the product type declared by `decl`.
  public init(_ decl: NodeID<ProductTypeDecl>, ast: AST) {
    self.decl = decl
    self.name = ast[decl].name
  }

  public var flags: TypeFlags { .isCanonical }

}

extension ProductType: CustomStringConvertible {

  public var description: String { name }

}
