/// The body of a function or subscript implementation.
public enum FunctionBody: Codable, Sendable {

  /// An expression body.
  case expr(AnyExprID)

  /// A block body.
  case block(BraceStmt.ID)

  /// The node wrapped by this instance.
  public var base: AnyNodeID {
    switch self {
    case .expr(let n):
      return AnyNodeID(n)
    case .block(let n):
      return AnyNodeID(n)
    }
  }

}
