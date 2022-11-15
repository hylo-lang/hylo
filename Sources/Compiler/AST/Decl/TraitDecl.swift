/// A trait declaration.
///
/// - Note: `TraitDecl` does not conform to `GenericDecl`.
public struct TraitDecl: TypeDecl, GenericScope {

  /// The access modifier of the declaration, if any.
  public private(set) var accessModifier: SourceRepresentable<AccessModifier>?

  /// The identifier of the trait.
  public let identifier: SourceRepresentable<Identifier>

  /// The names of traits which the trait refines.
  public let refinements: [NodeID<NameTypeExpr>]

  /// The member declarations in the lexical scope of the trait.
  public let members: [AnyDeclID]

  public init(
    identifier: SourceRepresentable<Identifier>,
    refinements: [NodeID<NameTypeExpr>] = [],
    members: [AnyDeclID] = []
  ) {
    self.identifier = identifier
    self.refinements = refinements
    self.members = members
  }

  public var name: String { identifier.value }

  /// Incorporates `accessModifier` into `self`.
  ///
  /// - Precondition: `self.accessModifier == nil`
  internal mutating func incorporate(_ accessModifier: SourceRepresentable<AccessModifier>) {
    precondition(self.accessModifier == nil)
    self.accessModifier = accessModifier
  }
}
