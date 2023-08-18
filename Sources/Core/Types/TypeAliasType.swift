import Utils

/// A type alias.
public struct TypeAliasType: TypeProtocol {

  /// The declaration that introduces the alias.
  public let decl: TypeAliasDecl.ID

  /// The name of the alias.
  public let name: Incidental<String>

  /// The target of the alias.
  public let aliasee: Incidental<AnyType>

  /// A set of flags describing recursive properties.
  public let flags: TypeFlags

  /// Creates a type alias resolving to `resolved` and declared by `d` in `ast`.
  public init(aliasing resolved: AnyType, declaredBy d: TypeAliasDecl.ID, in ast: AST) {
    self.decl = d
    self.name = Incidental(ast[decl].baseName)
    self.aliasee = Incidental(resolved)
    self.flags = resolved.flags.removing(.isCanonical)
  }

  /// The transitive aliasee of this alias.
  ///
  /// If `aliasee.value` is another alias `a`, this property is equal to `a.resolved`. Otherwise,
  /// it is equal to `aliasee.value`. In either case, `resolved` is not necessarily canonical.
  public var resolved: AnyType {
    if let a = TypeAliasType(aliasee.value) {
      return a.resolved
    } else {
      return aliasee.value
    }
  }

}

extension TypeAliasType: CustomStringConvertible {

  public var description: String { name.value }

}
