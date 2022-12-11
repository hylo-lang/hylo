/// A declaration that extends a type with new conformances.
public struct ConformanceDecl: TypeExtendingDecl {

  public let origin: SourceRange?

  /// The access modifier of the declaration, if any.
  public let accessModifier: SourceRepresentable<AccessModifier>?

  /// The expression of the extended type.
  public let subject: AnyTypeExprID

  /// The names of traits to which conformance is declared.
  public let conformances: [NodeID<NameExpr>]

  /// The condition of the conformance, if any.
  public let whereClause: SourceRepresentable<WhereClause>?

  /// The member declarations in the lexical scope of the conformance.
  public let members: [AnyDeclID]

  /// Creates an instance with the given properties.
  public init(
    accessModifier: SourceRepresentable<AccessModifier>?, subject: AnyTypeExprID,
    conformances: [NodeID<NameExpr>], whereClause: SourceRepresentable<WhereClause>?,
    members: [AnyDeclID], origin: SourceRange?
  ) {
    self.origin = origin
    self.accessModifier = accessModifier
    self.subject = subject
    self.conformances = conformances
    self.whereClause = whereClause
    self.members = members
  }

}
