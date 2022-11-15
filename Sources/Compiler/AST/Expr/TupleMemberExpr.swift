/// The expression of a tuple member, referred by its index.
public struct TupleMemberExpr: Expr {

  /// The parent tuple.
  public let tuple: AnyExprID

  /// The member's index.
  public let index: Int


  public init(tuple: AnyExprID, index: Int) {
    self.tuple = tuple
    self.index = index
  }

}

