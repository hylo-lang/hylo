import Utils

/// A module type.
public struct ModuleType: TypeProtocol, Hashable {

  /// The declaration that introduces the module.
  public let decl: NodeID<ModuleDecl>

  /// The name of the module.
  public let name: Incidental<String>

  public let flags: TypeFlags = .isCanonical

  public init(decl: NodeID<ModuleDecl>, ast: AST) {
    self.decl = decl
    self.name = Incidental(ast[decl].name)
  }

}

extension ModuleType: CustomStringConvertible {

  public var description: String { name.value }

}
