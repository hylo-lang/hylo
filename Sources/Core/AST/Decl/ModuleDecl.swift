/// A module declaration.
public struct ModuleDecl: SingleEntityDecl, LexicalScope {

  /// The name of the module.
  public let baseName: String

  /// The source files in the module.
  public private(set) var sources: [NodeID<TranslationUnit>] = []

  /// True iff this module has access to the Builtin module.
  public let canAccessBuiltins: Bool

  /// Creates an instance with the given properties and no source files.
  public init(name baseName: String, builtinModuleAccess canAccessBuiltins: Bool = false) {
    self.baseName = baseName
    self.site = SourceFile(synthesizedText: "/* module: \(baseName) */").wholeRange
    self.canAccessBuiltins = canAccessBuiltins
  }

  public let site: SourceRange

  /// Adds the given source file to our list of sources.
  public mutating func addSourceFile(_ s: NodeID<TranslationUnit>) {
    sources.append(s)
  }

}
