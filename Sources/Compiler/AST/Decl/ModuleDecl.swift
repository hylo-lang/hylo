/// A module declaration.
public struct ModuleDecl: Decl, ScopeOutliner {

  let scopeID: ScopeID = 0

  /// The next scope identifier.
  private var nextScopeID = 1

  /// The name of the module.
  public var name: String

  /// The member declarations in the lexical scope of the module.
  public var members: [AnyDeclIndex]

  public init(name: String, members: [AnyDeclIndex]) {
    self.name = name
    self.members = members
  }

  /// Returns a scope identifier guaranteed to be unique in this module.
  mutating func makeScopeID() -> ScopeID {
    defer { nextScopeID += 1 }
    return nextScopeID
  }

}
