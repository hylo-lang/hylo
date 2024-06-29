/// A trie (a.k.a. prefix tree).
public struct Trie<Key: Collection, Value> where Key.Element: Hashable {

  /// A node representing either one of the strings in a trie or a prefix thereof.
  fileprivate struct Node {

    /// The identifier of a node in a trie.
    typealias Identifier = Int

    ///The outgoing edges of a node.
    typealias Children = [Key.Element: Identifier]

    /// The outgoing edges of this node.
    var children: Children

    /// The value associated with the member string represented by `self`, if any.
    var value: Value?

    /// `true` iff `self` is not part of a member string.
    var isTombstone: Bool {
      children.isEmpty && (value == nil)
    }

  }

  /// The nodes in the trie, the first of which is the root.
  fileprivate var nodes: [Node]

  /// Creates an empty trie.
  public init() {
    self.nodes = [.init(children: [:], value: nil)]
  }

  /// The number of key/value pairs in `self`.
  public var count: Int {
    nodes.reduce(0, { (c, n) in c + (n.value == nil ? 0 : 1) })
  }

  /// `true` iff `self` is empty.
  public var isEmpty: Bool {
    nodes[0].isTombstone
  }

  /// Returns a collection with the key/value pairs in `self`.
  public var elements: Elements {
    .init(base: self, root: 0)
  }

  /// Returns a pair `(n, i)` such that `key[..<i]` is the longest key prefix contained in `self`
  /// and `n` is a sub-trie mapping the keys prefixed by `key[..<i]` to their corresponding value
  /// in `self`.
  ///
  /// If `key` is fully contained in one of the keys in `self`, then `i` is `key.endIndex` and `n`
  /// is the result of `self[prefix: key]`. If `key` shares no prefix with any of the keys stored
  /// in `self`, then `i` is `key.startIndex` and `n` is equal to `self`.
  public func longestPrefix<K: Collection>(
    startingWith key: K
  ) -> (trie: SubTrie<Key, Value>, position: K.Index) where K.Element == Key.Element {
    SubTrie(base: self, root: 0).longestPrefix(startingWith: key)
  }

  /// Accesses the value associated with `key`.
  public subscript<K: Collection>(key: K) -> Value? where K.Element == Key.Element {
    _read {
      if let n = find(key, from: 0) {
        yield nodes[n].value
      } else {
        yield nil
      }
    }

    _modify {
      var path: [(n: Node.Identifier, i: K.Index)] = []
      path.reserveCapacity(key.count)
      path.append((0, key.startIndex))

      while path.last!.i != key.endIndex {
        let (n, i) = path.last!
        if let m = nodes[n].children[key[i]] {
          path.append((m, key.index(after: i)))
        } else {
          break
        }
      }

      // There's already a node for `k`.
      if path.last!.i == key.endIndex {
        defer {
          while (path.count > 1) && nodes[path.last!.n].isTombstone {
            let (n, i) = path[path.count - 2]
            nodes[n].children.removeValue(forKey: key[i])
            path.removeLast()
          }
        }
        yield &nodes[path.last!.n].value
      }

      // There's no node for `k`.
      else {
        var value: Value? = nil
        defer {
          if value != nil {
            var (n, i) = path.last!
            while i != key.endIndex {
              nodes[n].children[key[i]] = nodes.count
              n = nodes.count
              i = key.index(after: i)
              nodes.append(.init(children: [:], value: nil))
            }
            nodes[nodes.count - 1].value = value
          }
        }
        yield &value
      }
    }
  }

  /// Returns a sub-trie mapping the keys prefixed by `key` to their corresponding value in `self`,
  /// or `nil` if `self` contains no key starting with by `key`.
  public subscript<K: Collection>(
    prefix key: K
  ) -> SubTrie<Key, Value>? where K.Element == Key.Element {
    SubTrie(base: self, root: 0)[prefix: key]
  }

  /// Returns the node corresponding to the longest prefix shared with `key` looked up from `root`.
  fileprivate func longestPrefix<K: Collection>(
    startingWith key: K, from root: Node.Identifier
  ) -> (node: Node.Identifier, position: K.Index) where K.Element == Key.Element {
    var n = root
    var i = key.startIndex
    while i != key.endIndex {
      if let m = nodes[n].children[key[i]] {
        n = m
        i = key.index(after: i)
      } else {
        break
      }
    }
    return (n, i)
  }

  /// Returns the node corresponding to `key` looked up from `root`.
  fileprivate func find<K: Collection>(
    _ key: K, from root: Node.Identifier
  ) -> Node.Identifier? where K.Element == Key.Element {
    let (n, i) = longestPrefix(startingWith: key, from: root)
    return i == key.endIndex ? n : nil
  }

}

extension Trie {

  /// A collection containing the elements of a `Trie` or `SubTrie`, in some arbitrary order.
  public struct Elements: Collection {

    /// A position in an instance of `Trie.Elements`.
    public struct Index: Hashable, Comparable {

      fileprivate struct Edge: Hashable, Comparable {

        var source: Node.Identifier

        var label: Node.Children.Index

        static func < (lhs: Self, rhs: Self) -> Bool {
          (lhs.source == rhs.source) && (lhs.label < rhs.label) || (lhs.source < rhs.source)
        }

      }

      fileprivate var path: [Edge]

      public static func < (lhs: Self, rhs: Self) -> Bool {
        // Empty path is the end index.
        if lhs.path.isEmpty {
          return false
        } else if rhs.path.isEmpty {
          return true
        } else {
          return lhs.path.lexicographicallyPrecedes(rhs.path)
        }
      }

    }

    /// A key/value pair stored in a `Trie`.
    public typealias Element = (key: [Key.Element], value: Value)

    /// The `Trie` of which `self` is a projection.
    fileprivate let base: Trie

    /// The root of the sub-tree of which `self` is a projection.
    fileprivate let root: Trie.Node.Identifier

    /// Returns the first index in `self`.
    public var startIndex: Index {
      if base.nodes[root].isTombstone { return endIndex }
      var path = [Index.Edge(source: root, label: base.nodes[root].children.startIndex)]
      extendPathToFirstLeaf(&path)
      return .init(path: path)
    }

    /// Returns the "past-the-end" index in `self`.
    public var endIndex: Index {
      return .init(path: [])
    }

    /// Returns the position immediately after `p`.
    ///
    /// - Precondition: `p` must be a valid index in `self` different from `endIndex`.
    public func index(after p: Index) -> Index {
      var nextPath: Array = p.path.dropLast()
      while !nextPath.isEmpty {
        let e = nextPath.count - 1
        nextPath[e].label = base.nodes[nextPath[e].source].children.index(after: nextPath[e].label)

        if nextPath[e].label != base.nodes[nextPath[e].source].children.endIndex {
          extendPathToFirstLeaf(&nextPath)
          return .init(path: nextPath)
        } else if base.nodes[nextPath[e].source].value != nil {
          return .init(path: nextPath)
        } else {
          nextPath.removeLast()
        }
      }
      return endIndex
    }

    /// Accesses the key/value pair at position `p`.
    ///
    /// - Precondition: `p` must be a valid index in `self` different from `endIndex`.
    public subscript(p: Index) -> (key: [Key.Element], value: Value) {
      let k = p.path.dropLast().map({ (edge) in base.nodes[edge.source].children[edge.label].key })
      let v = base.nodes[p.path.last!.source].value!
      return (key: k, value: v)
    }

    /// Extends `path` so to point at the first leaf of the node currently at the end of `path`.
    private func extendPathToFirstLeaf(_ path: inout [Index.Edge]) {
      while let p = path.last, p.label != base.nodes[p.source].children.endIndex {
        let m = base.nodes[p.source].children[p.label].value
        path.append(Index.Edge(source: m, label: base.nodes[m].children.startIndex))
      }
    }

  }

}

extension Trie: Equatable where Value: Equatable {

  /// Returns `true` iff `self` shares storage with `other`.
  private func isSharingStorage(with other: Self) -> Bool {
    self.nodes.withUnsafeBytes { (a) in
      other.nodes.withUnsafeBytes({ (b) in a.baseAddress == b.baseAddress })
    }
  }

  public static func == (lhs: Self, rhs: Self) -> Bool {
    // Fast path: `lhs` and `rhs` have the same storage.
    if lhs.isSharingStorage(with: rhs) { return true }

    // Slow path: check if both collections contain the same elements.
    for (k, v) in lhs.elements {
      if rhs[k] != v { return false }
    }
    return lhs.count == rhs.count
  }

}

extension Trie: Hashable where Value: Hashable {

  public func hash(into hasher: inout Hasher) {
    let copy = hasher
    var hash = copy.finalize()
    for (k, v) in elements {
      hash ^= k.hashValue ^ v.hashValue
    }
    hasher.combine(hash)
  }

}

extension Trie.Node: Equatable where Value: Equatable {}

extension Trie.Node: Hashable where Value: Hashable {}

extension Trie: CustomStringConvertible {

  public var description: String {
    return "[" + elements.map({ (k, v) in "\(k): \(v)" }).joined(separator: ", ") + "]"
  }

}

/// A part of a trie.
public struct SubTrie<Key: Collection, Value> where Key.Element: Hashable {

  /// The type of a trie projected by an instance of `Self`.
  public typealias Base = Trie<Key, Value>

  /// The `Trie` of which `self` is a projection.
  fileprivate let base: Base

  /// The root of this sub-tree.
  fileprivate let root: Base.Node.Identifier

  /// The number of key/value pairs in `self`.
  public var count: Int {
    var work = [root]
    var result = 0
    while let n = work.popLast() {
      if base.nodes[n].value != nil { result += 1 }
      work.append(contentsOf: base.nodes[n].children.values)
    }
    return result
  }

  /// Returns a collection with the key/value pairs in `self`.
  public var elements: Base.Elements {
    .init(base: base, root: root)
  }

  /// Accesses the value associated with `key`.
  public subscript<K: Collection>(key: K) -> Value? where K.Element == Key.Element {
    _read {
      if let n = base.find(key, from: root) {
        yield base.nodes[n].value
      } else {
        yield nil
      }
    }
  }

  /// Returns a sub-trie mapping the keys prefixed by `key` to their corresponding value in `self`,
  /// or `nil` if `self` contains no key starting with by `key`.
  public subscript<K: Collection>(prefix key: K) -> SubTrie? where K.Element == Key.Element {
    if let n = base.find(key, from: root) {
      return .init(base: base, root: n)
    } else {
      return nil
    }
  }

  /// Returns a pair `(n, i)` such that `key[..<i]` is the longest key prefix contained in `self`
  /// and `n` is a sub-trie mapping the keys prefixed by `key[..<i]` to their corresponding value
  /// in `self`.
  public func longestPrefix<K: Collection>(
    startingWith key: K
  ) -> (trie: SubTrie<Key, Value>, position: K.Index) where K.Element == Key.Element {
    let (n, i) = base.longestPrefix(startingWith: key, from: root)
    return (.init(base: base, root: n), i)
  }

}

extension SubTrie: CustomStringConvertible {

  public var description: String {
    return "[" + elements.map({ (k, v) in "\(k): \(v)" }).joined(separator: ", ") + "]"
  }

}
