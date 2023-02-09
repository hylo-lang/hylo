/// A module declaration.
public struct ModuleDecl: SingleEntityDecl, LexicalScope {

  /// The name of the module.
  public let baseName: String

  /// The source files in the module.
  public let sources: [NodeID<TopLevelDeclSet>]

  public init(name: String, sources: [NodeID<TopLevelDeclSet>]) {
    self.baseName = name
    self.sources = sources
    self.site = SourceFile(synthesizedText: "/* module: \(name) */").wholeRange
  }

  public let site: SourceRange

}
