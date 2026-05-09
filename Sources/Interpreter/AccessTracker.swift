import Utils
import FrontEnd

/// Tracks active accesses to parts of an object.
///
/// `PathComponent` describes one step in a path to a subobject. For example,
/// components for a tuple might be field indices, while components for a
/// struct might be property names.
///
/// Invariants:
///  - Accesses are active when they are created.
///  - If `x` is an active exclusive access (`sink`, `set`, or `inout`) of an
///    object `y`, no other access that includes `y` is active.
struct AccessTracker<PathComponent: Regular> {

  // The object `(b: (c: C, d: D), f: F)` is represented as:
  //             root
  //            /    \
  //           b      f
  //         /   \
  //        c     d
  // where each node has list of accesses it currently has.

  // Class invariants:
  //   1. Every node has at least one access or one child.
  //
  //   2. A `let` access is always active.
  //
  //   3. An `inout`/`set`/`sink` access is active iff:
  //      - it's the last element of `accessStack` of the corresponding node, and
  //      - the node has no descendants.

  /// A node representing a part of an object identified by a path prefix.
  ///
  /// Nodes form a tree where each node corresponds to a sequence of components
  /// (a prefix of a `Path`). Each node stores the accesses at that part
  /// and links to nodes representing its subparts.
  private struct Node {

    /// The component that identifies this node relative to its parent.
    let component: PathComponent

    /// Stack of accesses on the node.
    var accessStack: [Access]

    /// The indices of this node's children, each representing a further
    /// refinement of the current path.
    var children: [Index]

    /// Creates a node for `c` with no accesses or children.
    init(_ c: PathComponent) {
      self.component = c
      self.accessStack = []
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
  public init(_ object: PathComponent, with a: AccessEffect) {
    self.storage.append(Node(object))
    storage[0].accessStack.append(Access(kind: a))
  }

  /// An invalid access operation.
  public enum Error: Swift.Error, Regular {
    case overlappingExclusiveAccessExists(for: PathComponent)
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
  /// For example, given a structure `(b: (c: C, d: D), e: E)`, the path
  /// `[b, d]` identifies `d`.
  public typealias Path = ArraySlice<PathComponent>

  /// Starts a new access of kind `a` at `p`.
  public mutating func begin(_ a: AccessEffect, at p: Path) throws -> Access {
    let n = createNodesIfNeeded(for: p)
    return try begin(a, at: n)
  }

  /// Starts a new access of kind `a` for node at `i`.
  private mutating func begin(_ a: AccessEffect, at i: Index) throws -> Access {
    if !canBegin(a, at: i) {
      throw Error.overlappingExclusiveAccessExists(for: storage[i].component)
    }
    let r = Access(kind: a)
    self.storage[i].accessStack.append(r)
    return r
  }

  /// Ends `a` at `p`.
  ///
  /// - Precondition: `a` is present at the part identified by `p`.
  public mutating func end(_ a: Access, at p: Path) throws {
    try requireIsActive(a, in: p)

    let ns = indexPath(p)
    storage[ns.last!].accessStack.removeAll { $0 == a }
    removeEmptySuffix(from: ns)
  }

  /// Throws iff `a` at `p` is not active.
  ///
  /// - Precondition: `a` is present at the part identified by `p`.
  public func requireIsActive(_ a: Access, in p: Path) throws {
    if a.kind == .let { return }

    let i = nodeIndex(a, in: p)
    if !storage[i].children.isEmpty || storage[i].accessStack.last != a {
      throw Error.overlappingExclusiveAccessExists(for: storage[i].component)
    }
  }

  /// Returns the accesses encountered along the path `p`, including the root.
  ///
  /// - Precondition: `p` corresponds to a valid path.
  public func accesses(along p: Path) -> [[Access]] {
    indexPath(p).map { storage[$0].accessStack }
  }

  /// Returns the node corresponding to path `p`, creating any missing
  /// intermediate nodes along the path if needed.
  private mutating func createNodesIfNeeded(for p: Path) -> Index {
    p.reduce(0) { i, c in
        storage[i].children.first { storage[$0].component == c }
        ?? addChild(c, to: i)
    }
  }

  /// Returns the index of the first node along `p` that contains `a`.
  ///
  /// - Precondition: Some node visited while traversing `p` from the root
  ///   contains `a`.
  private func nodeIndex(_ a: Access, in p: Path) -> Index {
    var i = 0
    for c in p {
      if storage[i].accessStack.contains(a) { break }
      i = storage[i].children.first(where: { storage[$0].component == c })!
    }
    return i
  }

  /// Returns the indices of nodes along the longest existing prefix of `p`,
  /// starting from the root.
  ///
  /// The returned array always begins with the root index and has length equal
  /// to the number of matched path components plus one.
  private func indexPath(_ p: Path) -> [Index] {
    var r = [0]
    for c in p {
      guard let i = storage[r.last!].children.first(where: { storage[$0].component == c })
      else { break }
      r.append(i)
    }
    return r
  }

  /// Adds `c` as a child to the `i`th node and returns index of node corresponding
  /// to `c`.
  private mutating func addChild(_ c: PathComponent, to i: Index) -> Index {
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

  /// Returns true iff all nodes in subtree at `i` satisfy `predicate`.
  private func subtree(
    at i: Index,
    satisfies predicate: (Index) throws -> Bool
  ) rethrows -> Bool {
    try predicate(i) && (try storage[i].children.allSatisfy {
      try subtree(at: $0, satisfies: predicate)
    })
  }

  /// Removes the maximal suffix of `p` consisting of nodes that have neither
  /// active accesses nor children.
  private mutating func removeEmptySuffix(from p: [Index]) {
    var previousRemoved: Index? = nil
    for i in p.reversed() {
      storage[i].children.removeAll { $0 == previousRemoved }
      if !storage[i].accessStack.isEmpty || !storage[i].children.isEmpty {
        break
      }
      freeNodes.append(i)
      previousRemoved = i
    }
  }

  /// Returns whether `a` may begin at `i` without overlapping exclusive
  /// descendant accesses.
  private func canBegin(_ a: AccessEffect, at i: Index) -> Bool {
    if a == .let {
      return storage[i].children.allSatisfy {
        subtree(at: $0) {
          storage[$0].accessStack.first?.kind == .let
        }
      }
    } else {
      return storage[i].children.isEmpty
    }
  }

}
