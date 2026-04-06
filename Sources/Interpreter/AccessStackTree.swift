import FrontEnd
import Utils

/// An incorrect access operation.
public enum AccessError<Key: Regular>: Error {
  case canNotDerive(AccessKind, for: AccessStackTree<Key>.Path, from: Access, at: Key)
  case accessNotFound(Access)
  case pathNotFound(AccessStackTree<Key>.Path)
  case overlappingMutableAccessExists(for: Key)
  case activeDerivedAccessExists(for: Access, at: Key)
}

/// A tree indexed by hierarchical keys, where each node maintains a stack of
/// active accesses.
///
/// A descendent node is considered as "part-of" its ancestors.
/// Hence, ancestor/descendant relationships encode overlap.
public struct AccessStackTree<Key: Regular> {

  // Class Invariants:
  //
  // - Every node except the `root` has at least one child or at least one access,
  //   which is there for accomodating multiple trees.
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
  public mutating func add(_ a: AccessKind, at path: Path, derivedFrom p: Access?)
    throws -> Access
  {
    precondition(!path.isEmpty)
    let np = ensureNodePath(for: path)

    if let p = p {
      guard let start = np.firstIndex(where: { storage[$0].accesses.contains(p) }) else {
        throw AccessError<Key>.accessNotFound(p)
      }

      let derivedPath = Array(np[start...])

      guard canDerive(a, from: p, at: derivedPath) else {
        throw AccessError.canNotDerive(
          a,
          for: path,
          from: p,
          at: storage[derivedPath.first!].id!
        )
      }
    } else {
      if !canIntroduceAccess(a, at: np[0]) {
        throw AccessError.overlappingMutableAccessExists(for: storage[np[0]].id!)
      }
    }

    return try add(a, to: np.last!)
  }

  /// Ends `a` at `path`.
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
      throw AccessError<Key>.accessNotFound(a)
    }

    removeEmptySuffix(from: np)
  }

  /// Throws iff using `a` at `path` is conflicting with any other access.
  public func requireIsValid(_ a: Access, at path: Path) throws {
    precondition(!path.isEmpty)
    let i = try asNodePath(path).last!

    guard let j = storage[i].accesses.firstIndex(of: a) else {
      throw AccessError<Key>.accessNotFound(a)
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

  /// Returns true iff access of kind `a` can be derived from `p` for `path.last!`
  /// along `path`, which lists the positions from where `p` is active to the target.
  private func canDerive(_ a: AccessKind, from p: Access, at path: [Index]) -> Bool {
    precondition(!path.isEmpty)
    switch a {
    case .`let`:
      return canDeriveLetAccess(from: p, at: path)
    case .`set`:
      return canDeriveSetAccess(from: p, at: path)
    default:
      return (p.kind == .inout || p.kind == .sink) && canDeriveSetAccess(from: p, at: path)
    }
  }

  /// Returns true iff `let` access can be derived from `p` for `path.last!`
  /// along `path`, which lists the positions from where `p` is active to the target.
  private func canDeriveLetAccess(from p: Access, at path: [Index]) -> Bool {
    switch p.kind {
    case .set: false
    case .let: true
    default: storage[path[0]].accesses.last { $0.kind == .inout || $0.kind == .sink } == p
    }
  }

  /// Returns true iff `set` access can be derived from `p` for `path.last!`
  /// along `path`, which lists the positions from where `p` is active to the target.
  private func canDeriveSetAccess(from p: Access, at path: [Index]) -> Bool {
    storage[path[0]].accesses.noneSatisfy { $0.kind == .let }
      && storage[path[0]].accesses.last == p
      && path.dropFirst().allSatisfy { storage[$0].accesses.isEmpty }
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
  }

}
