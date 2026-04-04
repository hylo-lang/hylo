import FrontEnd

/// A hierarchical composition of access stacks, where each node tracks accesses
/// associated with an element.
public struct AccessStackTree<Element: Equatable> {

  // Invariants:
  //   - If `i`th node has `let` access on `j`th `accesses` index, then all accesses
  //     in its descendent subtree and all accesses of `i`th node after `j`th access
  //     are `let` access.
  //
  //  - Every node except `root` node in the tree has atleast one child or has atleast
  //    one access.

  /// The index of a node in storage.
  private typealias Index = Int

  /// A node in the tree.
  private struct Node {

    /// The element associated with this node.
    public let element: Element

    /// The accesses currently active on this node.
    public var accesses: [Access]

    /// The indices of this node's children.
    public var children: [Index]

    /// Creates a node with no `accesses` or `children`.
    public init(_ element: Element) {
      self.element = element
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

  /// Creates a tree with `root` as its root element.
  public init(root: Element) {
    storage.append(Node(root))
    self.root = 0
  }

  /// A path from the root of the tree to an element.
  ///
  /// Given a path `p`, `p[i]` identifies the element reached by extending
  /// the path `p[..<i]`. The empty path `[]` denotes the root element.
  ///
  /// For example, for a tree rooted at `A` with children `{B, C}`, where
  /// `C` has a child `{D}`:
  /// - `[B]` denotes `B`,
  /// - `[C, D]` denotes `D`, and
  /// - `[]` denotes `A`.
  public typealias Path = [Element]

  /// Adds an access of kind `a` derived from `p` at `path`, creating missing elements as needed
  /// and invalidating conflicting accesses in overlapping parts of the tree.
  public mutating func add(_ a: AccessKind, at path: Path, derivedFrom p: Access?)
    throws -> Access
  {
    precondition(!path.isEmpty)
    var i = root
    var d: [Index] = []
    var foundP = false
    for e in path {
      var j = storage[i].children.first { storage[$0].element == e }
      if j == nil {
        j = addChild(e, to: i)
      }
      i = j!
      if let p = p {
        if storage[i].accesses.contains(p) {
          foundP = true
        }
        if foundP {
          d.append(i)
        }
      }
    }

    if let p = p {
      if d.isEmpty {
        // throw error
      } else {
        try requireCanDerive(a, from: p, at: d)
      }
    }

    return try add(a, to: i)
  }

  /// Ends `a` at `path`.
  public mutating func end(_ a: Access, at path: Path) throws {
    precondition(!path.isEmpty)
    UNIMPLEMENTED()
  }

  /// Requires that the access `a` is valid for use at `path`,
  /// invalidating conflicting accesses in overlapping parts of the tree.
  public mutating func require(_ a: Access, at path: Path) throws {
    precondition(!path.isEmpty)
    UNIMPLEMENTED()
  }

  /// Adds `e` as child to node at `i`.
  private mutating func addChild(_ e: Element, to i: Index) -> Index {
    let r: Index

    if let last = free.popLast() {
      r = last
      storage[r] = Node(e)
    } else {
      r = storage.endIndex
      storage.append(Node(e))
    }

    storage[i].children.append(r)
    return r
  }

  /// Throws iff an access of kind `a` cannot be derived from `p` for `path.last!`
  /// along `path`, which lists the positions from where `p` is active to the target.
  private func requireCanDerive(
    _ a: AccessKind,
    from p: Access,
    at path: [Index]
  ) throws {
    precondition(!path.isEmpty)
    switch a {
    case .`let`:
      try requireCanDeriveLetAccess(from: p, at: path)
    case .`inout`:
      try requireCanDeriveInoutAccess(from: p, at: path)
    case .`set`:
      try requireCanDeriveSetAccess(from: p, at: path)
    case .sink:
      try requireCanDeriveSinkAccess(from: p, at: path)
    }
  }

  /// Throws iff `let` access cannot be derived from `p` for `path.last!`
  /// along `path`, which lists the positions from where `p` is active to the target.
  private func requireCanDeriveLetAccess(
    from p: Access,
    at path: [Index]
  ) throws {
  }

  /// Throws iff `inout` access cannot be derived from `p` for `path.last!`
  /// along `path`, which lists the positions from where `p` is active to the target.
  private func requireCanDeriveInoutAccess(
    from p: Access,
    at path: [Index]
  ) throws {
  }

  /// Throws iff `set` access cannot be derived from `p` for `path.last!`
  /// along `path`, which lists the positions from where `p` is active to the target.
  private func requireCanDeriveSetAccess(
    from p: Access,
    at path: [Index]
  ) throws {
  }

  /// Throws iff `sink` access cannot be derived from `p` for `path.last!`
  /// along `path`, which lists the positions from where `p` is active to the target.
  private func requireCanDeriveSinkAccess(
    from p: Access,
    at path: [Index]
  ) throws {
  }

  /// Adds access of kind `a` to node at `i`.
  private mutating func add(_ a: AccessKind, to i: Index) throws -> Access {
    UNIMPLEMENTED()
  }

}
