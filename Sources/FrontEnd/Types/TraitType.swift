import Utils

/// A trait type.
public struct TraitType: TypeProtocol {

  /// The declaration that introduces the trait.
  public let decl: TraitDecl.ID

  /// The name of the trait.
  public let name: Incidental<String>

  /// Creates an instance denoting the product type declared by `decl`.
  public init(_ decl: TraitDecl.ID, ast: AST) {
    self.decl = decl
    self.name = Incidental(ast[decl].baseName)
  }

  public var flags: ValueFlags { .init() }

}

extension TraitType: CustomStringConvertible {

  public var description: String { name.value }

}
