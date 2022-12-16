/// A sequence of lexical scopes, from inner to outer.
public struct LexicalScopeSequence: IteratorProtocol, Sequence {

  public typealias Element = AnyScopeID

  private let scopeToParent: ASTProperty<AnyScopeID>

  private var current: AnyScopeID?

  init(scopeToParent: ASTProperty<AnyScopeID>, current: AnyScopeID) {
    self.scopeToParent = scopeToParent
    self.current = current
  }

  public mutating func next() -> AnyScopeID? {
    guard let s = current else { return nil }
    current = scopeToParent[s]
    return s
  }

}
