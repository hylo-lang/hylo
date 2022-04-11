/// A module declaration.
public struct ModuleDecl: Decl, LexicalScope {

  public static let kind = NodeKind.moduleDecl

  /// The name of the module.
  public var name: String

  /// The member declarations in the lexical scope of the module.
  public var members: [AnyDeclIndex]

  public init(name: String, members: [AnyDeclIndex]) {
    self.name = name
    self.members = members
  }

}
