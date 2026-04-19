import FrontEnd
import Utils

/// Tracks active accesses to parts of an object. Enforces that no two
/// overlapping active exclusive accesses exist simultaneously.
///
/// Invariants:
///  - Accesses are active when they are created.
///  - If `x` is an active exclusive access (`sink`, `set`, or `inout`) of an
///    object `y`, no other access that includes `y` is active.
public struct AccessTracker<Component: Regular> {

  // Class invariants:
  //   1. Every node has at least one access or one child.
  //
  //   2. If a node contains an exclusive access `a` at index i in `accesses`,
  //      then `a` is active iff:
  //        - there is no access at any index `j` > `i` in `accesses`, and
  //        - the node has no descendants.
  //      Otherwise, `a` is inactive.

  /// A node representing a part of an object identified by a path prefix.
  ///
  /// Nodes form a tree where each node corresponds to a sequence of components
  /// (a prefix of a `Path`). Each node stores the accesses at that part
  /// and links to nodes representing its subparts.
  private struct Node {

    /// The component that identifies this node relative to its parent.
    let component: Component

    /// The accesses on this node.
    var accesses: [Access]

    /// The indices of this node's children, each representing a further
    /// refinement of the current path.
    var children: [Index]

    /// Creates a node for `c` with no accesses or children.
    init(_ c: Component) {
      self.component = c
      self.accesses = []
      self.children = []
    }

  }

  /// Storage of all nodes.
  private var storage: [Node] = []

  /// The index of a node in storage.
  private typealias Index = Int

  /// Indices in `storage` that are currently unused and may be reused.
  private var freeNodes: [Index] = []

  /// Creates a tracker for `object` with an initial access of kind `a`.
  public init(_ object: Component, with a: AccessKind) {
    self.storage.append(Node(object))
    storage[0].accesses.append(Access(kind: a))
  }

  /// An invalid access operation.
  public enum Error: Swift.Error {
    case overlappingExclusiveAccessExists(for: Component)
  }

  /// A path describing a part of an object.
  ///
  /// A `Path` is a sequence of `Component` values.
  ///
  /// - An empty sequence identifies the whole object being tracked.
  /// - Each subsequent element identifies a component of the region selected
  ///   by the previous elements.
  ///
  /// A path therefore describes a sequence of nested components, starting
  /// from a specific object and refining down to a subcomponent.
  ///
  /// For example, given a structure `A { B { C, D }, E }`, the path
  /// `[B, D]` identifies `D`.
  public typealias Path = [Component]

  /// Starts a new access of kind `a` at `p`.
  public mutating func begin(_ a: AccessKind, at p: Path) throws -> Access {
    let n = createNodesIfNeeded(for: p)
    return try begin(a, at: n)
  }

  /// Ends `a` at `p`.
  ///
  /// - Precondition: `a` is present at the part identified by `p`.
  public mutating func end(_ a: Access, at p: Path) throws {
    try requireIsActive(a, at: p)

    let ns = nodePath(of: p)
    storage[ns.last!].accesses.removeAll { $0 == a }
    removeEmptySuffix(from: ns)
  }

  /// Throws iff `a` at `p` is not active.
  ///
  /// - Precondition: `a` is present at the part identified by `p`.
  public func requireIsActive(_ a: Access, at p: Path) throws {
    if a.kind == .let { return }

    let i = index(of: p)
    if !storage[i].children.isEmpty || storage[i].accesses.last != a {
      throw Error.overlappingExclusiveAccessExists(for: storage[i].component)
    }
  }

  /// Returns the node corresponding to path `p`, creating any missing
  /// intermediate nodes along the path if needed.
  private mutating func createNodesIfNeeded(for p: Path) -> Index {
    p.reduce(into: 0) { i, c in
      i =
        storage[i].children.first { storage[$0].component == c }
        ?? addChild(c, to: i)
    }
  }

  /// Returns the index of the node corresponding to `p`.
  ///
  /// - Precondition: Nodes for all prefixes of `p` exist in the tree.
  private func index(of p: Path) -> Index {
    p.reduce(into: 0) { i, c in
      i = storage[i].children.first { storage[$0].component == c }!
    }
  }

  /// Returns the indices of nodes along the path `p`, starting from the root.
  ///
  /// The returned array has length `p.count + 1`, where the first element
  /// is the root node.
  private func nodePath(of p: Path) -> [Index] {
    p.reduce(into: [0]) { r, c in
      let i = storage[r.last!].children.first { storage[$0].component == c }!
      r.append(i)
    }
  }

  /// Adds `c` as child to node at `i`.
  private mutating func addChild(_ c: Component, to i: Index) -> Index {
    let r: Index

    if let last = freeNodes.popLast() {
      r = last
      storage[r] = Node(c)
    } else {
      r = storage.endIndex
      storage.append(Node(c))
    }

    storage[i].children.append(r)
    return r
  }

  /// Starts a new access of kind `a` for node at `i`.
  private mutating func begin(_ a: AccessKind, at i: Index) throws -> Access {
    let hasNoOverlap =
      if a == .let {
        storage[i].children.allSatisfy {
          subtree(at: $0) { storage[$0].accesses.first?.kind == .let }
        }
      } else {
        storage[i].children.isEmpty
      }
    if !hasNoOverlap {
      throw Error.overlappingExclusiveAccessExists(for: storage[i].component)
    }

    let r = Access(kind: a)
    self.storage[i].accesses.append(r)
    return r
  }

  /// Returns true iff all nodes in subtree at `i` satisfy `predicate`.
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
  private mutating func removeEmptySuffix(from p: [Index]) {
    var previousRemoved: Index? = nil
    for i in p.reversed() {
      storage[i].children.removeAll { $0 == previousRemoved }
      if !storage[i].accesses.isEmpty || !storage[i].children.isEmpty {
        break
      }
      freeNodes.append(i)
      previousRemoved = i
    }
  }

}
