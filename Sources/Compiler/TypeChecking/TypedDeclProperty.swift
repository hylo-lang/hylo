/// A mapping from typed declarations to properties of a given type.
public struct TypedDeclProperty<Value> {

  /// The internal storage of the property map.
  private var storage: DeclProperty<Value>

  /// Creates an empty node map.
  public init() {
    self.storage = DeclProperty<Value>()
  }

  /// Accesses the property associated with the specified ID.
  public subscript<ID: DeclID>(decl: TypedNode<ID>) -> Value? {
    _read { yield storage[decl.id] }
    _modify { yield &storage[decl.id] }
  }

  /// Accesses the property associated with the specified ID, using a default value.
  public subscript<ID: DeclID>(
    decl: TypedNode<ID>,
    default defaultValue: @autoclosure () -> Value
  ) -> Value {
    _read { yield storage[decl.id, default: defaultValue()] }
    _modify { yield &storage[decl.id, default: defaultValue()] }
  }

}

extension TypedDeclProperty: Equatable where Value: Equatable {}

extension TypedDeclProperty: Hashable where Value: Hashable {}

extension TypedDeclProperty: Codable where Value: Codable {}
