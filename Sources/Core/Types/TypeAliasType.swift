import Utils

/// A type alias.
public struct TypeAliasType: TypeProtocol {

  /// The declaration that introduces the alias.
  public let decl: TypeAliasDecl.ID

  /// The name of the alias.
  public let name: Incidental<String>

  /// The resolved type of the alias.
  public let resolved: Incidental<AnyType>

  /// A set of flags describing recursive properties.
  public let flags: TypeFlags

  /// Creates a type alias resolving to `resolved` and declared by `d` in `ast`.
  public init(aliasing resolved: AnyType, declaredBy d: TypeAliasDecl.ID, in ast: AST) {
    self.decl = d
    self.name = Incidental(ast[decl].baseName)
    self.resolved = Incidental(resolved)
    self.flags = resolved.flags.removing(.isCanonical)
  }

}

extension TypeAliasType: CustomStringConvertible {

  public var description: String { name.value }

}
