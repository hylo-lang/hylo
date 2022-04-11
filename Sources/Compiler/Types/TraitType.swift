/// The type of a trait declaration.
public struct TraitType: TypeProtocol, Hashable {

  /// The declaration that introduces the trait.
  public let decl: NodeIndex<TraitDecl>

  public let flags: TypeFlags = .isCanonical

  public func canonical() -> Type { .trait(self) }

}
