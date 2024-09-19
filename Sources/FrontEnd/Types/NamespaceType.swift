import Utils

/// The type of a namespace declaration.
public struct NamespaceType: TypeProtocol {

  /// The declaration that introduces the namespace.
  public let decl: NamespaceDecl.ID

  /// The name of the product type.
  public let name: Incidental<String>

  /// Creates an instance denoting the type declared by `decl`.
  public init(_ decl: NamespaceDecl.ID, ast: AST) {
    self.decl = decl
    self.name = Incidental(ast[decl].baseName)
  }

  public var flags: ValueFlags { .init() }

}

extension NamespaceType: CustomStringConvertible {

  public var description: String { name.value }

}
