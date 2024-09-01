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
  public let flags: ValueFlags

  /// Creates a type alias resolving to `aliasee` and declared by `d` in `ast`.
  public init(aliasing aliasee: AnyType, declaredBy d: TypeAliasDecl.ID, in ast: AST) {
    self.init(decl: d, name: ast[d].baseName, aliasee: aliasee)
  }

  /// Creates an instance with the given properties.
  private init(decl: TypeAliasDecl.ID, name: String, aliasee: AnyType) {
    self.decl = decl
    self.name = Incidental(name)
    self.aliasee = Incidental(aliasee)
    self.flags = aliasee.flags | .hasNonCanonical
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

  /// Applies `TypeProtocol.transform(mutating:_:)` on `m` and the types that are part of `self`.
  public func transformParts<M>(
    mutating m: inout M, _ transformer: (inout M, AnyType) -> TypeTransformAction
  ) -> Self {
    let a = aliasee.value.transform(mutating: &m, transformer)
    return TypeAliasType(decl: decl, name: name.value, aliasee: a)
  }

}

extension TypeAliasType: CustomStringConvertible {

  public var description: String { name.value }

}
