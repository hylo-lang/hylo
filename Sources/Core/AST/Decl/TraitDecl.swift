/// A trait declaration.
///
/// - Note: `TraitDecl` does not conform to `GenericDecl`.
public struct TraitDecl: SingleEntityDecl, TypeScope, GenericScope {

  public let site: SourceRange

  /// The access modifier of the declaration, if any.
  public let accessModifier: SourceRepresentable<AccessModifier>?

  /// The identifier of the trait.
  public let identifier: SourceRepresentable<Identifier>

  /// The names of traits which the trait refines.
  public let refinements: [NodeID<NameExpr>]

  /// The member declarations in the lexical scope of the trait.
  public let members: [AnyDeclID]

  /// The declaration of the trait's `Self` parameter.
  public let selfParameterDecl: NodeID<GenericParameterDecl>

  /// Creates an instance with the given properties.
  public init(
    accessModifier: SourceRepresentable<AccessModifier>?,
    identifier: SourceRepresentable<Identifier>,
    refinements: [NodeID<NameExpr>],
    members: [AnyDeclID],
    selfParameterDecl: NodeID<GenericParameterDecl>,
    site: SourceRange
  ) {
    precondition(members.contains(AnyDeclID(selfParameterDecl)))

    self.site = site
    self.accessModifier = accessModifier
    self.identifier = identifier
    self.refinements = refinements
    self.selfParameterDecl = selfParameterDecl
    self.members = members
  }

  public var baseName: String { identifier.value }

}
