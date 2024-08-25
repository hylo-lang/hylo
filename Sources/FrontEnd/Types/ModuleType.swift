import Utils

/// A module type.
public struct ModuleType: TypeProtocol {

  /// The declaration that introduces the module.
  public let decl: ModuleDecl.ID

  /// The name of the module.
  public let name: Incidental<String>

  /// Creates an instance denoting the module declared by `decl`.
  public init(_ decl: ModuleDecl.ID, ast: AST) {
    self.decl = decl
    self.name = Incidental(ast[decl].baseName)
  }

  public var flags: ValueFlags { .init() }

}

extension ModuleType: CustomStringConvertible {

  public var description: String { name.value }

}
