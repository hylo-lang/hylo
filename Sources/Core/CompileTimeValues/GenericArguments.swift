import OrderedCollections
import Utils

/// A map from generic parameter to its argument.
public struct GenericArguments {

  /// A key in this map.
  public typealias Key = GenericParameterDecl.ID

  /// A value in this map.
  public typealias Value = any CompileTimeValue

  /// The type of this map's contents.
  fileprivate typealias Contents = OrderedDictionary<GenericParameterDecl.ID, Value>

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
    self.contents = .init(uniqueKeysWithValues: keysAndValues)
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

  /// Returns a new map containing the keys of `self` with the values transformed `transform`.
  public func mapValues(_ transform: (Value) throws -> Value) rethrows -> Self {
    return try .init(contents: contents.mapValues(transform))
  }

  /// Returns this argument list appended with `suffix`.
  ///
  /// - Requires: `self` does not define a value for any of the values defined in `suffix`.
  public func appending(_ suffix: Self) -> Self {
    // Note: `merging` perserves order.
    let c = contents.merging(suffix.contents, uniquingKeysWith: { (_, _) in unreachable() })
    return .init(contents: c)
  }

}

extension GenericArguments: Equatable {

  public static func == (l: Self, r: Self) -> Bool {
    l.contents.elementsEqual(r.contents) { (a, b) in
      (a.key == b.key) && a.value.equals(b.value)
    }
  }
}

extension GenericArguments: Hashable {

  public func hash(into hasher: inout Hasher) {
    for (k, v) in contents {
      k.hash(into: &hasher)
      v.hash(into: &hasher)
    }
  }

}

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
    contents.elements[position]
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
