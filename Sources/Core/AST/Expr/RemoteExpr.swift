/// The type expression of a remote type or value (e.g., `remote let Int`).
public struct RemoteExpr: Expr {

  public let site: SourceRange

  /// The site of the expression's introducer.
  public let introducerSite: SourceRange

  /// The passing convention of the remote type.
  public var convention: SourceRepresentable<AccessEffect>

  /// The expression of the projected type.
  public let operand: AnyExprID

  /// Creates an instance with the given properties.
  public init(
    introducerSite: SourceRange,
    convention: SourceRepresentable<AccessEffect>,
    operand: AnyExprID,
    site: SourceRange
  ) {
    self.site = site
    self.introducerSite = introducerSite
    self.convention = convention
    self.operand = operand
  }

}
