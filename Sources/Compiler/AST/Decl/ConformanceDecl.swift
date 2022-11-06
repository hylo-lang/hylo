/// A declaration that extends a type with new conformances.
public struct ConformanceDecl: TypeExtendingDecl {

  public static let kind = NodeKind.conformanceDecl

  /// The access modifier of the declaration, if any.
  public private(set) var accessModifier: SourceRepresentable<AccessModifier>?

  /// The expression of the extended type.
  public let subject: AnyTypeExprID

  /// The names of traits to which conformance is declared.
  public let conformances: [NodeID<NameTypeExpr>]

  /// The condition of the conformance, if any.
  public let whereClause: SourceRepresentable<WhereClause>?

  /// The member declarations in the lexical scope of the conformance.
  public let members: [AnyDeclID]

  /// Creates an instance with the given properties and no `accessModifier`.
  public init(
    accessModifier: SourceRepresentable<AccessModifier>? = nil,
    subject: AnyTypeExprID,
    conformances: [NodeID<NameTypeExpr>],
    whereClause: SourceRepresentable<WhereClause>?,
    members: [AnyDeclID]
  ) {
    self.accessModifier = accessModifier
    self.subject = subject
    self.conformances = conformances
    self.whereClause = whereClause
    self.members = members
  }

  /// Incorportates `accessModifer` into `self`.
  public mutating func incorporate(_ accessModifier: SourceRepresentable<AccessModifier>?) {
    precondition(self.accessModifier == nil)
    self.accessModifier = accessModifier
  }
}
