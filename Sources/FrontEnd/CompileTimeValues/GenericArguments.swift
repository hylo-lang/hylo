import OrderedCollections
import Utils

/// A map from generic parameter to its argument.
public struct GenericArguments: Hashable {

  /// A key in this map.
  public typealias Key = GenericParameterDecl.ID

  /// A value in this map.
  public typealias Value = CompileTimeValue

  /// The type of this map's contents.
  public typealias Contents = [GenericParameterDecl.ID: Value]

  /// The contents of `self`.
  fileprivate var contents: Contents

  /// Creates an instance with given `contents`.
  private init(contents: Contents) {
    self.contents = contents
  }

  /// Creates an empty instance.
  public init() {
    self.contents = [:]
  }

  /// Creates a new map with the arguments of `t`.
  public init(_ t: BoundGenericType) {
    self.contents = .init(uniqueKeysWithValues: t.arguments.lazy.map({ (k, v) in (k, v) }))
  }

  /// Creates an instance mapping each element of `parameters`, which is defined in `ast`, to its
  /// skolemized form.
  public init<S: Sequence>(
    skolemizing parameters: S, in ast: AST
  ) where S.Element == GenericParameterDecl.ID {
    self.contents = .init(
      uniqueKeysWithValues: parameters.map { (p) in
        (key: p, value: .type(^GenericTypeParameterType(p, ast: ast)))
      })
  }

  /// A collection with the parameters to which arguments are passed.
  public var keys: some Collection<Key> {
    contents.keys
  }

  /// A collection with the values of the arguments.
  public var values: some Collection<Value> {
    contents.values
  }

  /// Accesses the value associated with the given key.
  public subscript(key: Key) -> Value? {
    get { contents[key] }
    set { contents[key] = newValue }
  }

  /// Accesses the value associated with the given key.
  public subscript(key: Key, default value: @autoclosure () -> Value) -> Value {
    contents[key, default: value()]
  }

  /// Returns a new map containing the keys of `self` with the values transformed `transform`.
  public func mapValues(_ transform: (Value) throws -> Value) rethrows -> Self {
    try .init(contents: contents.mapValues(transform))
  }

  /// Returns `self` merged with `other`, applying `combine` to determine the value of any
  /// duplicate key.
  public func merging(_ other: Self, uniquingKeysWith combine: (Value, Value) -> Value) -> Self {
    var clone = self
    clone.contents.merge(other.contents, uniquingKeysWith: combine)
    return clone
  }

  /// Returns `self` merged with `other`, asserting that duplicate keys have the same value.
  public func merging(_ other: Self) -> Self {
    merging(other) { (a, b) in
      assert(a == b)
      return a
    }
  }

  /// Appends `suffix` to `self`.
  ///
  /// - Requires: `self` does not define a value for any of the values defined in `suffix`.
  public mutating func append(_ suffix: Self) {
    // Note: `merging` preserves order.
    contents.merge(suffix.contents, uniquingKeysWith: { (_, _) in unreachable() })
  }

  /// An empty instance.
  public static var empty: Self = .init()

}

extension GenericArguments: Collection {

  public typealias Element = (key: Key, value: Value)

  public typealias Index = Contents.Index

  public var startIndex: Index {
    contents.startIndex
  }

  public var endIndex: Index {
    contents.endIndex
  }

  public func index(after position: Index) -> Index {
    contents.index(after: position)
  }

  public subscript(position: Index) -> Element {
    contents[position]
  }

}

extension GenericArguments: CustomStringConvertible {

  public var description: String {
    String(describing: contents)
  }

}
