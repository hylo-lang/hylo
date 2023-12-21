/// A generic clause.
public struct GenericClause: Codable {

  /// The parameters of the clause.
  public let parameters: [GenericParameterDecl.ID]

  /// The where clause of the generic clause, if any.
  public let whereClause: SourceRepresentable<WhereClause>?

  /// Creates an instance with the given properties.
  public init(
    parameters: [GenericParameterDecl.ID],
    whereClause: SourceRepresentable<WhereClause>?
  ) {
    self.parameters = parameters
    self.whereClause = whereClause
  }

}
