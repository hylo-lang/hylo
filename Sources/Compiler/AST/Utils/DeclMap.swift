/// A mapping from indices of declarations to properties of a given type.
public struct DeclMap<Value> {

  /// The internal storage of the map.
  private var storage: [Int: Value]

  /// Creates an empty node map.
  public init() {
    storage = [:]
  }

  /// Accesses the property associated with the specified index.
  subscript<T: Decl>(index: NodeIndex<T>) -> Value? {
    _read   { yield storage[index.rawValue] }
    _modify { yield &storage[index.rawValue] }
  }

  /// Accesses the property associated with the specified index.
  subscript(index: AnyDeclIndex) -> Value? {
    _read   { yield storage[index.rawValue] }
    _modify { yield &storage[index.rawValue] }
  }

}

extension DeclMap: Equatable where Value: Equatable {}

extension DeclMap: Hashable where Value: Hashable {}
