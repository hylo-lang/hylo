/// A module declaration.
public struct ModuleDecl: Decl, LexicalScope {

  public static let kind = NodeKind.moduleDecl

  /// The name of the module.
  public var name: String

  /// The source files in the module.
  public var sources: [NodeID<TopLevelDeclSet>]

  public init(name: String, sources: [NodeID<TopLevelDeclSet>] = []) {
    self.name = name
    self.sources = sources
  }

}
