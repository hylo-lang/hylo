/// A generic clause.
public struct GenericClause: Hashable {

  /// The parameters of the clause.
  public var parameters: [GenericParamDeclID]

  /// The where clause of the generic clause, if any.
  public var whereClause: SourceRepresentable<WhereClause>?

  public init(
    parameters: [GenericParamDeclID],
    whereClause: SourceRepresentable<WhereClause>? = nil
  ) {
    self.parameters = parameters
    self.whereClause = whereClause
  }

}
