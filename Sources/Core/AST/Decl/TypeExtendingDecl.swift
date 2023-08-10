/// A declaration that extends a type.
public protocol TypeExtendingDecl: ExposableDecl, GenericScope {

  /// The expression of the extended type.
  var subject: AnyTypeExprID { get }

  /// The condition of the extension, if any.
  var whereClause: SourceRepresentable<WhereClause>? { get }

}
