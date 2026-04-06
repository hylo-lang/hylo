import FrontEnd
import Utils

/// An incorrect access operation.
public enum AccessError<Key: Regular>: Error, Regular {
  case incompatibleDerivation(of: AccessKind, from: AccessKind)
  case cannotDerive(AccessKind, for: Key, from: Access, at: Key, dueToAccess: Access, on: Key)
  case accessNotFound(Access, inPathTo: Key)
  case pathNotFound(AccessStackForest<Key>.Path)
  case overlappingMutableAccessExists(for: Key)
  case activeDerivedAccessExists(for: Access, at: Key)
}

/// A forest of access stacks indexed by hierarchical keys.
///
/// Each path identifies a node in a conceptual tree. Nodes that do not share
/// a common prefix are independent, so the structure behaves as a collection
/// of disjoint trees (a forest).
///
/// A descendent node is considered as "part-of" its ancestors. Hence,
/// ancestor/descendant relationships encode overlap.
public struct AccessStackForest<Key: Regular> {

  // Class Invariants:
  //
  // - Every node except the root has at least one child or at least one access.
  //   The root is a synthetic node that serves as the anchor for otherwise
  //   independent trees.
  //
  // - If the `i`th node has a `.let` access at index `j`, then all accesses in its
  //   descendant subtree and all accesses of the `i`th node after index `j` are `.let` accesses.
  //
  // - If the `i`th node has an `.inout`, `.set`, or `.sink` access at index `j`, then
  //   all accesses in its descendant subtree and all accesses of the `i`th node after
  //   index `j` are directly or indirectly derived from that access.

  /// The index of a node in storage.
  private typealias Index = Int

  /// A node in the tree.
  private struct Node {

    /// Identity of a node.
    public let id: Key?

    /// The accesses currently active on this node.
    public var accesses: [Access]

    /// The indices of this node's children.
    public var children: [Index]

    /// Creates a node with no `accesses` or `children`.
    public init(_ key: Key?) {
      self.id = key
      accesses = []
      children = []
    }

  }

  /// The storage of all nodes in the tree.
  private var storage: [Node] = []

  /// The indices of nodes that may be reused.
  private var free: [Index] = []

  /// The index of the root node.
  private var root: Index

  /// Creates an empty tree.
  public init() {
    storage.append(Node(nil))
    self.root = 0
  }

  /// A sequence of keys identifying a position in the tree.
  ///
  /// A path `[k0, k1, ..., kn]` denotes the node reached by successively
  /// extending from the root with each key. The empty path `[]` denotes
  /// the root.
  ///
  /// Paths are canonical: a given sequence identifies a unique node.
  public typealias Path = [Key]

  /// A path in the tree expressed as node indices.
  private typealias NodePath = [Index]

  /// Adds an access of kind `a` derived from `p` at `path`, creating missing
  /// elements as needed.
  ///
  /// - Precondition: Each key in `path` appears only along that path.
  /// - Precondition: if `p` is `nil`, then `path.count == 1`
  ///
  /// - Postcondition: Provides strong exception safety guarantees.
  public mutating func add(_ a: AccessKind, at path: Path, derivedFrom p: Access?)
    throws -> Access
  {
    precondition(!path.isEmpty)
    let np = ensureNodePath(for: path)

    if let p = p {
      guard let start = np.firstIndex(where: { storage[$0].accesses.contains(p) }) else {
        throw AccessError<Key>.accessNotFound(p, inPathTo: path.last!)
      }

      let derivedPath = Array(np[start...])
      if let i = storage[derivedPath[0]].accesses.firstIndex(of: p) {
        try requireCanDerive(a, fromAccessAt: i, at: derivedPath)
      } else {
        throw AccessError.accessNotFound(p, inPathTo: path.last!)
      }
    } else {
      if !canIntroduceAccess(a, at: np[0]) {
        throw AccessError.overlappingMutableAccessExists(for: storage[np[0]].id!)
      }
    }

    return try add(a, to: np.last!)
  }

  /// Ends `a` at `path`.
  ///
  /// - Postcondition: Provides strong exception safety guarantees.
  public mutating func end(_ a: Access, at path: Path) throws {
    precondition(!path.isEmpty)
    let np = try asNodePath(path)
    let i = np.last!

    if a.kind != .let && !storage[i].children.isEmpty {
      throw AccessError.activeDerivedAccessExists(for: a, at: storage[i].id!)
    }

    if let j = storage[i].accesses.firstIndex(where: { $0 == a }) {
      if a.kind != .let && j != storage[i].accesses.endIndex - 1 {
        throw AccessError.activeDerivedAccessExists(for: a, at: storage[i].id!)
      }
      storage[i].accesses.remove(at: j)
    } else {
      throw AccessError<Key>.accessNotFound(a, inPathTo: path.last!)
    }

    removeEmptySuffix(from: np)
  }

  /// Throws iff using `a` at `path` is conflicting with any other access.
  public func requireIsUsable(_ a: Access, at path: Path) throws {
    precondition(!path.isEmpty)
    let i = try asNodePath(path).last!

    guard let j = storage[i].accesses.firstIndex(of: a) else {
      throw AccessError<Key>.accessNotFound(a, inPathTo: path.last!)
    }

    if a.kind != .let && (j != storage[i].accesses.endIndex - 1 || !storage[i].children.isEmpty) {
      throw AccessError.activeDerivedAccessExists(for: a, at: storage[i].id!)
    }
  }

  /// Returns the `NodePath` corresponding to `p`.
  private func asNodePath(_ p: Path) throws -> NodePath {
    var i = root
    var r: NodePath = []

    for e in p {
      guard let j = storage[i].children.first(where: { storage[$0].id == e }) else {
        throw AccessError<Key>.pathNotFound(p)
      }
      i = j
      r.append(i)
    }

    return r
  }

  /// Returns the `NodePath` corresponding to `p`, creating missing nodes as needed.
  private mutating func ensureNodePath(for p: Path) -> NodePath {
    var i = root
    var r: NodePath = []

    for e in p {
      let j =
        storage[i].children.first(where: { storage[$0].id == e })
        ?? addChild(e, to: i)
      i = j
      r.append(i)
    }

    return r
  }

  /// Adds `k` as child to node at `i`.
  private mutating func addChild(_ k: Key, to i: Index) -> Index {
    let r: Index

    if let last = free.popLast() {
      r = last
      storage[r] = Node(k)
    } else {
      r = storage.endIndex
      storage.append(Node(k))
    }

    storage[i].children.append(r)
    return r
  }

  /// Throws iff access of kind `a` can not be derived from `storage[path[0]].accesses[i]`
  /// for `path.last!` along `path`, which lists the positions from where `p`
  /// is active to the target.
  private func requireCanDerive(_ a: AccessKind, fromAccessAt i: Int, at path: [Index]) throws {
    precondition(!path.isEmpty)
    try require(a, canBeDerivedFrom: storage[path[0]].accesses[i].kind)

    let p: (Access) -> Bool =
      switch a {
      case .let: { $0.kind != .let }
      default: { _ in true }
      }
    if let (x, y) = firstAccess(along: path, startingFrom: i + 1, where: p) {
      throw AccessError.cannotDerive(
        a, for: storage[path.last!].id!, from: storage[path[0]].accesses[i],
        at: storage[path[0]].id!, dueToAccess: y, on: storage[x].id!)
    }
  }

  /// Throws iff it is access of kind `a` can not be derived from access of kind `p`.
  private func require(_ a: AccessKind, canBeDerivedFrom p: AccessKind) throws {
    let areIncompatible =
      switch a {
      case .let: p == .set
      case .set: p == .let
      default: p == .let || p == .set
      }
    if areIncompatible {
      throw AccessError<Key>.incompatibleDerivation(of: a, from: p)
    }
  }

  /// Returns true iff an access of kind `a` can be introduced at node `i`
  /// without being derived from an existing access.
  private func canIntroduceAccess(_ a: AccessKind, at i: Index) -> Bool {
    switch a {
    case .let:
      return storage[i].accesses.allSatisfy { $0.kind == .let }
    default:
      return storage[i].accesses.isEmpty
    }
  }

  /// Adds access of kind `a` to node at `i`.
  private mutating func add(_ a: AccessKind, to i: Index) throws -> Access {
    func p(_ i: Index) -> Bool {
      switch a {
      case .let: storage[i].accesses.isEmpty || storage[i].accesses.first!.kind == .let
      default: storage[i].accesses.isEmpty
      }
    }
    let b = storage[i].children.allSatisfy { subtree(at: $0, satisfies: p) }
    if !b {
      throw AccessError.overlappingMutableAccessExists(for: storage[i].id!)
    }

    let r = Access(kind: a)
    self.storage[i].accesses.append(r)
    return r
  }

  /// Returns true iff all nodes in subtree at `i` satisfies `predicate`.
  private func subtree(
    at i: Index,
    satisfies predicate: (Index) throws -> Bool
  ) rethrows -> Bool {
    if try !predicate(i) { return false }

    return try storage[i].children.allSatisfy {
      try subtree(at: $0, satisfies: predicate)
    }
  }

  /// Removes the maximal suffix of `p` consisting of nodes that have neither
  /// active accesses nor children.
  ///
  /// - Precondition: `p` never contains 0.
  private mutating func removeEmptySuffix(from p: NodePath) {
    var previousRemoved: Index? = nil
    for i in p.reversed() {
      if let j = storage[i].children.firstIndex(where: { $0 == previousRemoved }) {
        storage[i].children.remove(at: j)
      }
      if !storage[i].accesses.isEmpty || !storage[i].children.isEmpty {
        break
      }
      free.append(i)
      previousRemoved = i
    }

    if let j = storage[0].children.firstIndex(where: { $0 == previousRemoved }) {
      storage[0].children.remove(at: j)
    }
  }

  /// Returns the first access along `p` satisfying `predicate`, starting at the
  /// `i`th access of `p[0]`.
  private func firstAccess(
    along p: [Index],
    startingFrom i: Int,
    where predicate: (Access) -> Bool
  ) -> (node: Index, access: Access)? {
    precondition(!p.isEmpty)

    if let r = storage[p[0]].accesses[i...].first(where: { predicate($0) }) {
      return (p[0], r)
    }

    for n in p.dropFirst() {
      if let r = storage[n].accesses.first(where: { predicate($0) }) {
        return (n, r)
      }
    }

    return nil
  }

}
