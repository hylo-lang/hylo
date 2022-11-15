/// An associated value declaration.
public struct AssociatedValueDecl: SingleEntityDecl {

  /// The identifier of the type.
  public let identifier: SourceRepresentable<Identifier>

  /// The where clause of the declaration, if any.
  public let whereClause: SourceRepresentable<WhereClause>?

  /// The default value of the declaration, if any.
  public let defaultValue: AnyExprID?

  public init(
    identifier: SourceRepresentable<Identifier>,
    whereClause: SourceRepresentable<WhereClause>? = nil,
    defaultValue: AnyExprID? = nil
  ) {
    self.identifier = identifier
    self.whereClause = whereClause
    self.defaultValue = defaultValue
  }

  public var name: String { identifier.value }

}
