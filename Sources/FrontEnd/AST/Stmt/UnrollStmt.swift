/// A loop that is unrolled at compile time.
public struct UnrollStmt: Stmt, LexicalScope {

  public let site: SourceRange

  /// The site of the `unroll` introducer.
  public let introducerSite: SourceRange

  /// - Requires `count` >= 0
  public let count: Int

  /// The body of the loop.
  public let body: BraceStmt.ID

  public init(
    introducerSite: SourceRange,
    count: Int,
    body: BraceStmt.ID,
    site: SourceRange
  ) {
    precondition(count >= 0)

    self.site = site
    self.introducerSite = introducerSite
    self.count = count
    self.body = body
  }

}
