import Utils

/// A nominal product type.
public struct ProductType: TypeProtocol {

  /// The declaration that introduces the type.
  public let decl: ProductTypeDecl.ID

  /// The name of the product type.
  public let name: Incidental<String>

  public let flags: TypeFlags

  /// Creates an instance denoting the product type declared by `decl`.
  public init(_ decl: ProductTypeDecl.ID, ast: AST) {
    self.decl = decl
    self.name = Incidental(ast[decl].baseName)
    self.flags = TypeFlags.isCanonical
  }

}

extension ProductType: CustomStringConvertible {

  public var description: String { name.value }

}
