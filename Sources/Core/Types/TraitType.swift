import Utils

/// A trait type.
public struct TraitType: TypeProtocol {

  /// The declaration that introduces the trait.
  public let decl: NodeID<TraitDecl>

  /// The name of the trait.
  public let name: Incidental<String>

  /// Creates an instance denoting the product type declared by `decl`.
  public init(_ decl: NodeID<TraitDecl>, ast: AST) {
    self.decl = decl
    self.name = Incidental(ast[decl].name)
  }

  public var flags: TypeFlags { .isCanonical }

}

extension TraitType: CustomStringConvertible {

  public var description: String { name.value }

}
