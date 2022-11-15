/// A trait declaration.
///
/// - Note: `TraitDecl` does not conform to `GenericDecl`.
public struct TraitDecl: TypeDecl, GenericScope {

  /// The access modifier of the declaration, if any.
  public let accessModifier: SourceRepresentable<AccessModifier>?

  /// The identifier of the trait.
  public let identifier: SourceRepresentable<Identifier>

  /// The names of traits which the trait refines.
  public let refinements: [NodeID<NameTypeExpr>]

  /// The member declarations in the lexical scope of the trait.
  public let members: [AnyDeclID]

  /// Creates an instance with the given properties.
  public init(
    accessModifier: SourceRepresentable<AccessModifier>?,
    identifier: SourceRepresentable<Identifier>,
    refinements: [NodeID<NameTypeExpr>],
    members: [AnyDeclID]
  ) {
    self.accessModifier = accessModifier
    self.identifier = identifier
    self.refinements = refinements
    self.members = members
  }

  public var name: String { identifier.value }

}
