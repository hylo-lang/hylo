/// The type expression of a remote type (e.g., `remote let Int`).
public struct RemoteTypeExpr: Expr {

  public let site: SourceRange

  /// The source range of the expression's introducer.
  public let introducerRange: SourceRange

  /// The passing convention of the remote type.
  public var convention: SourceRepresentable<AccessEffect>

  /// The expression of the projected type.
  public let operand: AnyTypeExprID

  public init(
    introducerRange: SourceRange,
    convention: SourceRepresentable<AccessEffect>,
    operand: AnyTypeExprID,
    site: SourceRange
  ) {
    self.site = site
    self.introducerRange = introducerRange
    self.convention = convention
    self.operand = operand
  }

}
