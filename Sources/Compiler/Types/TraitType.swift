import Utils

/// A trait type.
public struct TraitType: TypeProtocol, Hashable {

  /// The declaration that introduces the trait.
  public let decl: NodeID<TraitDecl>

  /// The name of the trait.
  public let name: Incidental<String>

  public let flags: TypeFlags = .isCanonical

  public init(decl: NodeID<TraitDecl>, ast: AST) {
    self.decl = decl
    self.name = Incidental(ast[decl].name)
  }

}

extension TraitType: CustomStringConvertible {

  public var description: String { name.value }

}
