/// A generic clause.
public struct GenericClause: Codable {

  /// The parameters of the clause.
  public let parameters: [GenericParamDeclID]

  /// The where clause of the generic clause, if any.
  public let whereClause: SourceRepresentable<WhereClause>?

  public init(
    parameters: [GenericParamDeclID],
    whereClause: SourceRepresentable<WhereClause>? = nil
  ) {
    self.parameters = parameters
    self.whereClause = whereClause
  }

}
