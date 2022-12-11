/// A generic clause.
public struct GenericClause: Codable {

  /// The parameters of the clause.
  public let parameters: [NodeID<GenericParameterDecl>]

  /// The where clause of the generic clause, if any.
  public let whereClause: SourceRepresentable<WhereClause>?

  public init(
    parameters: [NodeID<GenericParameterDecl>], whereClause: SourceRepresentable<WhereClause>? = nil
  ) {
    self.parameters = parameters
    self.whereClause = whereClause
  }

}
