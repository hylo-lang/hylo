/// The type expression of a stored projection.
public struct StoredProjectionTypeExpr: TypeExpr {

  public struct Introducer: SourceRepresentable {

    public enum Kind {

      case `let`

      case `inout`

    }

    public var range: SourceRange?

    public var kind: Kind

  }

  public var range: SourceRange?

  public var introducer: Introducer

  /// The expression of the projected type.
  public var operand: TypeExpr

}
