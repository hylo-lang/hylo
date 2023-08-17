/// A module declaration.
public struct ModuleDecl: SingleEntityDecl, LexicalScope {

  public static let constructDescription = "module declaration"

  /// The name of the module.
  public let baseName: String

  /// The source files in the module.
  public let sources: [TranslationUnit.ID]

  /// True iff this module has access to the Builtin module.
  public let canAccessBuiltins: Bool

  public let site: SourceRange

  /// Creates an instance with the given properties and no source files.
  public init(
    _ baseName: String,
    sources: [TranslationUnit.ID],
    builtinModuleAccess canAccessBuiltins: Bool = false
  ) {
    self.baseName = baseName
    self.sources = sources
    self.canAccessBuiltins = canAccessBuiltins
    self.site = SourceFile(synthesizedText: "/* module: \(baseName) */").wholeRange
  }

}
