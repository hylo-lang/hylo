/// An explicit cast expression.
public struct CastExpr: Expr {

  /// The direction of a cast expression w.r.t. the type lattice.
  public enum Direction: Codable {

    /// An upcast.
    case up

    /// An downcast.
    case down

    /// A built-in pointer conversion.
    ///
    /// - Note: built-in conversion expressions may only be used in the core library. The compiler
    ///   shall emit a warning if one is found outside of core library sources.
    case builtinPointerConversion

  }

  public let site: SourceRange

  /// The left operand.
  public let left: AnyExprID

  /// The type to which the left operand is being converted.
  public let right: AnyTypeExprID

  /// The direction of the cast.
  public let direction: Direction

  /// Creates an instance with the given properties.
  public init(left: AnyExprID, right: AnyTypeExprID, direction: Direction, site: SourceRange) {
    self.site = site
    self.left = left
    self.right = right
    self.direction = direction
  }

}
