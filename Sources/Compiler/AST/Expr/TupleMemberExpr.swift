/// The expression of a tuple member, referred by its index.
public struct TupleMemberExpr: Expr {

  public static let kind = NodeKind.tupleMemberExpr

  /// The parent tuple.
  public var tuple: AnyExprID

  /// The member's index.
  public var index: Int


  public init(tuple: AnyExprID, index: Int) {
    self.tuple = tuple
    self.index = index
  }

}

