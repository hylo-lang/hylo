/// A generic clause.
public struct GenericClause: Hashable {

  /// The parameters of the clause.
  public var params: [GenericParamDeclID]

  /// The where clause of the generic clause, if any.
  public var whereClause: SourceRepresentable<WhereClause>?

}
