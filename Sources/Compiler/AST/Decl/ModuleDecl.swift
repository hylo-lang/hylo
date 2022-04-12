/// A module declaration.
public struct ModuleDecl: Decl, LexicalScope {

  public static let kind = NodeKind.moduleDecl

  /// The name of the module.
  public var name: String

  /// The member declarations in the lexical scope of the module.
  public var members: [AnyDeclID]

  public init(name: String, members: [AnyDeclID]) {
    self.name = name
    self.members = members
  }

}
