/// A sequence of lexical scopes, from inner to outer.
public struct LexicalScopeSequence: IteratorProtocol, Sequence {

  public typealias Element = AnyScopeID

  /// A map from scope to its parent scope.
  private let scopeToParent: ASTProperty<AnyScopeID>

  /// The next scope returned by `next()`.
  private var nextScope: AnyScopeID?

  /// Creates an instance iterating over `s` and its ancestors, reading scope relationships from
  /// `scopeToParent`.
  init<S: ScopeID>(scopeToParent: ASTProperty<AnyScopeID>, from s: S) {
    self.scopeToParent = scopeToParent
    self.nextScope = AnyScopeID(s)
  }

  /// Advances to the next scope and returns it, or `nil` if no next scope exists.
  public mutating func next() -> AnyScopeID? {
    guard let s = nextScope else { return nil }
    nextScope = scopeToParent[s]
    return s
  }

  /// Returns first scope in the sequence that has type `s`, or `nil` if no such scope exists.
  public func first<S: LexicalScope & Node>(_ s: S.Type) -> S.ID? {
    first(transformedBy: S.ID.init(_:))
  }

}
