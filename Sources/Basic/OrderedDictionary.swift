/// A dictionary that keeps track of the order in which key-value pairs are inserted.
///
/// This is roughly a drop-in replacement for Swift's `Dictionary`, which additionally remembers
/// the order in which new key-value pairs are inserted.
public struct OrderedDictionary<Key, Value> where Key: Hashable {

  /// Creates a new empty dictionary.
  public init() {
    contents = [:]
    keys = []
  }

  /// Creates a new dictionary from the key-value pairs in the given sequence.
  ///
  /// - Parameter keysAndValues: A sequence of key-value pairs to use for. Every key in
  ///   `keysAndValues` must be unique.
  public init<S>(uniqueKeysWithValues keysAndValues: S)
  where S: Sequence, S.Element == (Key, Value)
  {
    contents = [:]
    contents.reserveCapacity(keysAndValues.underestimatedCount)
    keys = []
    keys.reserveCapacity(keysAndValues.underestimatedCount)

    for (key, value) in keysAndValues {
      precondition(contents[key] == nil)
      contents[key] = value
      keys.append(key)
    }
  }

  /// The underlying dictionary containing each key-value pair.
  private var contents: [Key: Value]

  /// A collection containing just the keys of the dictionary, in order.
  public private(set) var keys: [Key]

  /// A collection containing just the values of the dictionary, in order.
  public var values: [Value] { keys.map({ key in contents[key]! }) }

  /// A Boolean value that indicates whether the dictionary is empty.
  public var isEmpty: Bool { keys.isEmpty }

  /// The number of key-value pairs in the dictionary.
  public var count: Int { keys.count }

  /// Accesses the value associated with the given key for reading and writing.
  ///
  /// - Parameter key: The key identifying the value to find.
  public subscript(key: Key) -> Value? {
    get { contents[key] }
    set {
      if let value = newValue {
        if contents.updateValue(value, forKey: key) == nil {
          keys.append(key)
        }
      } else {
        if contents.removeValue(forKey: key) != nil {
          keys.remove(at: keys.firstIndex(of: key)!)
        }
      }
    }
  }

  /// Accesses the value associated with the given key for reading and writing, using a default
  /// value if the key does not exist in the dictionary.
  ///
  /// - Parameters:
  ///   - key: The key identifying the value to find.
  ///   - defaultValue: The default value to use if `key` does not exist in the dictionary.
  public subscript(key: Key, default defaultvalue: @autoclosure () -> Value) -> Value {
    get { contents[key, default: defaultvalue()] }
    set { self[key] = newValue }
  }

  /// Returns the index for the given key.
  ///
  /// - Parameter key: The key to find in the dictionary.
  public func index(forKey key: Key) -> Int? {
    return keys.firstIndex(of: key)
  }

}

extension OrderedDictionary: BidirectionalCollection {

  public typealias Index = Int
  public typealias Element = (key: Key, value: Value)

  public var startIndex: Int { 0 }

  public var endIndex: Int { keys.count }

  public func index(after i: Int) -> Int {
    return i + 1
  }

  public func index(before i: Int) -> Int {
    return i - 1
  }

  public subscript(position: Int) -> (key: Key, value: Value) {
    let key = keys[position]
    return (key: key, value: contents[key]!)
  }

}

extension OrderedDictionary: ExpressibleByDictionaryLiteral {

  public init(dictionaryLiteral elements: (Key, Value)...) {
    self.init(uniqueKeysWithValues: elements)
  }

}

extension OrderedDictionary: CustomStringConvertible {

  public var description: String {
    let keysAndValues = map({ key, value in "\(key): \(value)" }).joined(separator: ", ")
    return "[\(keysAndValues)]"
  }

}
