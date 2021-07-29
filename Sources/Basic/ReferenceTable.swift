/// A dictionary whose keys are object with identities.
public struct ReferenceTable<Key, Value> where Key: AnyObject {

  private var storage: [HashableBox<Key, ReferenceHashWitness<Key>>: Value]

  /// Creates an empty table.
  public init() {
    self.storage = [:]
  }

  /// Creates a new table from the key-value pairs in the given sequence.
  public init<S>(uniqueKeysWithValues keysAndValues: S)
  where S: Sequence, S.Element == (Key, Value)
  {
    self.storage = Dictionary(
      uniqueKeysWithValues: keysAndValues.map({ key, value in (HashableBox(key), value) }))
  }

  public subscript(key: Key) -> Value? {
    get     { storage[HashableBox(key)] }
    set     { storage[HashableBox(key)] = newValue }
    _modify { yield &storage[HashableBox(key)] }
  }

  /// A collection containing just the keys of the table.
  public var keys: [Key] {
    return storage.keys.map({ $0.value })
  }

  /// A collection contining just the values of the table.
  public var values: Dictionary<HashableBox<Key, ReferenceHashWitness<Key>>, Value>.Values {
    return storage.values
  }

}

extension ReferenceTable: Collection {

  public typealias Index = Dictionary<HashableBox<Key, ReferenceHashWitness<Key>>, Value>.Index

  public typealias Element = (key: Key, value: Value)

  public var startIndex: Index { storage.startIndex }

  public var endIndex: Index { storage.endIndex }

  public func index(after i: Index) -> Index {
    return storage.index(after: i)
  }

  public subscript(position: Index) -> (key: Key, value: Value) {
    let (box, value) = storage[position]
    return (box.value, value)
  }

}

extension ReferenceTable: ExpressibleByDictionaryLiteral {

  public init(dictionaryLiteral elements: (Key, Value)...) {
    self.init(uniqueKeysWithValues: elements)
  }

}
