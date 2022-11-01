/// A set of declarations at the top-level of a source file.
public struct TopLevelDeclSet: Node, LexicalScope {

  public static let kind = NodeKind.topLevelDeclSet

  /// The declarations in the set.
  public private(set) var decls: [AnyDeclID]

  public init(decls: [AnyDeclID] = []) {
    self.decls = decls
  }

  /// Adds `d` to `self`.
  internal mutating func add(_ d: AnyDeclID) {
    decls.append(d)
  }
}
