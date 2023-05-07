import OrderedCollections
import Utils

/// A map from generic parameter to its argument.
public struct GenericArguments {

  /// A key in this map.
  public typealias Key = GenericParameterDecl.ID

  /// A value in this map.
  public typealias Value = any CompileTimeValue

  /// A value wrapped into an equatable box.
  fileprivate typealias _Value = HashableBox<CompileTimeValueHashableWitness>

  /// The type of this map's contents.
  fileprivate typealias Contents = OrderedDictionary<GenericParameterDecl.ID, _Value>

  /// The contents of `self`
  fileprivate var contents: Contents

  /// Creates an instance with given `contents`.
  private init(contents: Contents) {
    self.contents = contents
  }

  /// Creates an empty dictionary.
  public init() {
    self.contents = [:]
  }

  /// Creates a new map from the key-value pairs in the given sequence.
  public init<S: Sequence>(uniqueKeysWithValues keysAndValues: S) where S.Element == (Key, Value) {
    self.contents = .init(uniqueKeysWithValues: Self.adapt(keysAndValues))
  }

  /// A collection with the values of the arguments.
  public var values: some Collection<Value> {
    contents.values.lazy.map(\.base)
  }

  /// Accesses the value associated with the given key.
  public subscript(key: Key) -> Value? {
    get { contents[key]?.base }
    set { contents[key] = newValue.map(_Value.init(_:)) }
  }

  /// Returns a new map containing the keys of `self` with the values transformed `transform`.
  public func mapValues(_ transform: (Value) throws -> Value) rethrows -> Self {
    return try .init(contents: contents.mapValues({ try _Value(transform($0.base)) }))
  }

  /// Returns this argument list appended with `suffix`.
  ///
  /// - Requires: `self` does not define a value for any of the values defined in `suffix`.
  public func appending(_ suffix: Self) -> Self {
    // Note: `merging` perserves order.
    let s = suffix.lazy.map({ (key: $0.key, value: _Value($0.value)) })
    return .init(contents: contents.merging(s, uniquingKeysWith: { (_, _) in unreachable() }))
  }

  /// Returns a sequence containing the given `keysAndValues` with the values wrapped in `_Value`.
  private static func adapt<S: Sequence>(
    _ keysAndValues: S
  ) -> LazyMapSequence<S, (Key, _Value)> where S.Element == (Key, Value) {
    keysAndValues.lazy.map({ ($0, _Value($1)) })
  }

}

extension GenericArguments: Hashable {}

extension GenericArguments: ExpressibleByDictionaryLiteral {

  public init(dictionaryLiteral elements: (Key, Value)...) {
    self.init(uniqueKeysWithValues: elements)
  }

}

extension GenericArguments: Collection {

  public typealias Element = (key: Key, value: Value)

  public typealias Index = Int

  public var startIndex: Index { 0 }

  public var endIndex: Index { contents.count }

  public func index(after position: Index) -> Index {
    position + 1
  }

  public subscript(position: Index) -> Element {
    read(contents.elements[position], { (key: $0.key, value: $0.value.base) })
  }

}

extension GenericArguments: CustomStringConvertible {

  public var description: String {
    String(describing: contents)
  }

}

/// A type serving as a witness for `Constraint`s conformance to `Hashable`.
private struct CompileTimeValueHashableWitness: HashableWitness {

  typealias Element = any CompileTimeValue

  static func hash(_ constraint: Element, into hasher: inout Hasher) {
    constraint.hash(into: &hasher)
  }

  static func isEqual(_ left: Element, to right: Element) -> Bool {
    left.equals(right)
  }

}
