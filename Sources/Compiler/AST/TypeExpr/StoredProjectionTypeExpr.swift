/// The type expression of a stored projection.
public struct StoredProjectionTypeExpr: TypeExpr {

  public enum Introducer: Codable {

    case `let`

    case `inout`

  }

  public var introducer: SourceRepresentable<Introducer>

  /// The expression of the projected type.
  public let operand: AnyTypeExprID

  public init(
    introducer: SourceRepresentable<StoredProjectionTypeExpr.Introducer>,
    operand: AnyTypeExprID
  ) {
    self.introducer = introducer
    self.operand = operand
  }

}
