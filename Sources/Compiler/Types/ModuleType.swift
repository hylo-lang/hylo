/// The type of a module declaration.
public struct ModuleType: TypeProtocol, Hashable {

  /// The declaration that introduces the module.
  public let decl: NodeIndex<ModuleDecl>

  public let flags: TypeFlags = .isCanonical

  public func canonical() -> Type { .module(self) }

}
