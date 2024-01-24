/// The type expression of a remote type (e.g., `remote let Int`).
public struct RemoteTypeExpr: Expr {

  /// The site from which `self` was parsed.
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
