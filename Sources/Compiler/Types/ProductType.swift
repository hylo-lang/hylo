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

extension ProductType {

  /// Returns the product type named `name` from the standard library `ast.stdlib`, or `nil` such a
  /// type does not exists.
  ///
  /// - Requires: The standard library must be loaded and assigned to `ast.stlib`.
  public init?(standardLibraryTypeNamed name: String, ast: AST) {
    let stdlib = ast.stdlib ?? preconditionFailure("standard library is not loaded")
    for id in ast.topLevelDecls(stdlib) where id.kind == .productTypeDecl {
      let id = NodeID<ProductTypeDecl>(converting: id)!
      if ast[id].name == name {
        self.init(decl: id, ast: ast)
        return
      }
    }
    return nil
  }

}

extension ProductType: CustomStringConvertible {

  public var description: String { name.value }

}
