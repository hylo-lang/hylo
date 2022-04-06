/// A generic clause.
public struct GenericClause: SourceRepresentable {

  public var range: SourceRange?

  /// The parameters of the clause.
  public var params: [GenericParamDeclIndex]

  /// The where clause of the generic clause, if any.
  public var whereClause: WhereClause?

}
