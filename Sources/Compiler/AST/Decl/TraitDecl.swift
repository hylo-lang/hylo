/// A trait declaration.
public struct TraitDecl: SingleEntityDecl, GenericScope {

  public static let kind = NodeKind.traitDecl

  /// The access modifier of the declaration, if any.
  public var access: SourceRepresentable<AccessModifier>?

  /// The identifier of the trait.
  public var identifier: SourceRepresentable<Identifier>

  /// The names of traits which the trait refines.
  public var refinements: [NodeID<NameTypeExpr>]

  /// The member declarations in the lexical scope of the trait.
  public var members: [AnyDeclID]

  public var name: String { identifier.value }

}
