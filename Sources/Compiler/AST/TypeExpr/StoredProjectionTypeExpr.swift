/// The type expression of a stored projection.
public struct StoredProjectionTypeExpr: Hashable {

  public enum Introducer: Hashable {

    case `let`

    case `inout`

  }

  public var introducer: SourceRepresentable<Introducer>

  /// The expression of the projected type.
  public var operand: SourceRepresentable<TypeExpr>

}
