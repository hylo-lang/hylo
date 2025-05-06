extension AST {

  /// The loaded entities from Hylo's standard library that are known to the compiler.
  public var core: CoreEntities {
    CoreEntities(ast: self)
  }

}

/// A helper to project core entities from an AST.
@dynamicMemberLookup
public struct CoreEntities: Sendable {

  /// The AST in which the entities are defined.
  fileprivate let ast: AST

  /// Projects `m` bundled with `ast`.
  public subscript<T>(dynamicMember m: KeyPath<CoreTraits, T>) -> CoreTrait<T> {
    CoreTrait(ast: ast, entity: ast.coreTraits![keyPath: m])
  }

}

/// An entity declared in Hylo's standard library.
@dynamicMemberLookup
public struct CoreTrait<T: CoreTraitDescription> {

  /// The AST in which the entity is defined.
  fileprivate let ast: AST

  /// A description of this entity's declaration.
  fileprivate let entity: T

  /// The definition of the trait.
  public var type: TraitType {
    TraitType(entity.decl, ast: ast)
  }

  /// Projects the entity description at `m`.
  public subscript<R>(dynamicMember m: KeyPath<T, R>) -> R {
    entity[keyPath: m]
  }

}
