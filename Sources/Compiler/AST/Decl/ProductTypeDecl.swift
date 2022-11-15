/// A (nominal) product type declaration.
public struct ProductTypeDecl: GenericDecl, TypeDecl, GenericScope {

  /// The access modifier of the declaration, if any.
  public private(set) var accessModifier: SourceRepresentable<AccessModifier>?

  /// The identifier of the type.
  public let identifier: SourceRepresentable<Identifier>

  /// The generic clause of the declaration, if any.
  public let genericClause: SourceRepresentable<GenericClause>?

  /// The names of traits to which the type conforms.
  public let conformances: [NodeID<NameTypeExpr>]

  /// The member declarations in the lexical scope of the trait.
  public let members: [AnyDeclID]

  /// The memberwise initializer of the type.
  public let memberwiseInit: NodeID<FunDecl>

  public init(
    identifier: SourceRepresentable<Identifier>,
    genericClause: SourceRepresentable<GenericClause>?,
    conformances: [NodeID<NameTypeExpr>],
    members: [AnyDeclID],
    memberwiseInit: NodeID<FunDecl>
  ) {
    precondition(members.contains(AnyDeclID(memberwiseInit)))
    self.identifier = identifier
    self.genericClause = genericClause
    self.conformances = conformances
    self.members = members
    self.memberwiseInit = memberwiseInit
  }

  public var name: String { identifier.value }

  /// Returns whether the declaration is public.
  public var isPublic: Bool { accessModifier?.value != nil }

  /// Incorporates `accessModifier` into `self`.
  ///
  /// - Precondition: `self.accessModifier == nil`
  internal mutating func incorporate(_ accessModifier: SourceRepresentable<AccessModifier>) {
    precondition(self.accessModifier == nil)
    self.accessModifier = accessModifier
  }

}
