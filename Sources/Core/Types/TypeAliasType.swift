import Utils

/// A type alias.
public struct TypeAliasType: TypeProtocol {

  /// The declaration that introduces the alias.
  public let decl: TypeAliasDecl.ID

  /// The name of the alias.
  public let name: Incidental<String>

  /// The resolved type of the alias.
  public let resolved: Incidental<AnyType>

  public let flags: TypeFlags

  /// Creates a type alias resolving to `resolved` and declared by `d` in `ast`.
  public init(aliasing resolved: AnyType, declaredBy d: TypeAliasDecl.ID, in ast: AST) {
    self.decl = d
    self.name = Incidental(ast[decl].baseName)
    self.resolved = Incidental(resolved)

    var flags = resolved.flags.removing(.isCanonical)
    if ast[decl].genericClause != nil {
      flags.insert(.isGeneric)
    } else {
      flags.remove(.isGeneric)
    }
    self.flags = flags
  }

}

extension TypeAliasType: CustomStringConvertible {

  public var description: String { name.value }

}
