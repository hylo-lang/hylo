import Core
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
  fileprivate typealias Contents = [GenericParameterDecl.ID: _Value]

  /// The contents of `self`
  fileprivate var contents: Contents

  /// Creates an empty dictionary.
  public init() {
    self.contents = [:]
  }

  /// Creates a new map from the key-value pairs in the given sequence.
  public init<S: Sequence>(uniqueKeysWithValues keysAndValues: S) where S.Element == (Key, Value) {
    self.contents = .init(uniqueKeysWithValues: Self.adapt(keysAndValues))
  }

  /// Accesses the value associated with the given key.
  public subscript(key: Key) -> Value? {
    get { contents[key]?.base }
    set { contents[key] = newValue.map(_Value.init(_:)) }
  }

  /// Merges the key-value pairs in `other` into `self` using `combine` to determine the value for
  /// any duplicate keys.
  public mutating func merge<S: Sequence>(
    _ other: S, uniquingKeysWith combine: (Value, Value) throws -> Value
  ) rethrows where S.Element == (Key, Value) {
    try contents.merge(Self.adapt(other)) { (a, b) in
      try .init(combine(a.base, b.base))
    }
  }

  /// Merges `other` into `self` using `combine` to determine the value for any duplicate keys.
  public mutating func merge(
    _ other: Self, uniquingKeysWith combine: (Value, Value) throws -> Value
  ) rethrows {
    try contents.merge(other.contents) { (a, b) in
      try .init(combine(a.base, b.base))
    }
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

  public struct Index: Comparable {

    fileprivate let value: Contents.Index

    public static func < (l: Self, r: Self) -> Bool {
      l.value < r.value
    }

  }

  public var startIndex: Index {
    .init(value: contents.startIndex)
  }

  public var endIndex: Index {
    .init(value: contents.endIndex)
  }

  public func index(after position: Index) -> Index {
    .init(value: contents.index(after: position.value))
  }

  public subscript(position: Index) -> Element {
    read(contents[position.value], { (key: $0.key, value: $0.value.base) })
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
