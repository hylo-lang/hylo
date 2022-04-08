/// A while loop.
public struct WhileStmt: ScopeOutliner, Hashable {

  var scopeID: ScopeID

  /// The condition of the loop.
  public var condition: [SourceRepresentable<ConditionItem>]

  /// The body of the loop.
  public var body: SourceRepresentable<BraceStmt>

}
