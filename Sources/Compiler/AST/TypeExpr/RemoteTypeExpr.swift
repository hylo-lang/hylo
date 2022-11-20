/// The type expression of a remote type (e.g., `remote let Int`).
public struct RemoteTypeExpr: TypeExpr {

  /// The source range of the expression's introducer, if any.
  public let introducerRange: SourceRange?

  /// The passing convention of the remote type.
  public var convention: SourceRepresentable<PassingConvention>

  /// The expression of the projected type.
  public let operand: AnyTypeExprID

  public init(
    introducerRange: SourceRange?,
    convention: SourceRepresentable<PassingConvention>,
    operand: AnyTypeExprID
  ) {
    self.introducerRange = introducerRange
    self.convention = convention
    self.operand = operand
  }

}
