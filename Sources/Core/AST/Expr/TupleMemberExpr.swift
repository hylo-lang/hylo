/// The expression of a tuple member, referred by its index.
public struct TupleMemberExpr: Expr {

  public let site: SourceRange

  /// The parent tuple.
  public let tuple: AnyExprID

  /// The member's index.
  public let index: SourceRepresentable<Int>

  public init(tuple: AnyExprID, index: SourceRepresentable<Int>, site: SourceRange) {
    self.site = site
    self.tuple = tuple
    self.index = index
  }

}
