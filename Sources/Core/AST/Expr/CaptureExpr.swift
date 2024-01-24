/// The capture of an access on a value.
public struct CaptureExpr: Expr {

  /// The site from which `self` was parsed.
  public let site: SourceRange

  /// The capability being captured.
  public var access: SourceRepresentable<AccessEffect>

  /// The expression of the value being captured.
  public let source: AnyExprID

  /// Creates an instance with the given properties.
  public init(
    access: SourceRepresentable<AccessEffect>,
    source: AnyExprID,
    site: SourceRange
  ) {
    self.site = site
    self.access = access
    self.source = source
  }

}
