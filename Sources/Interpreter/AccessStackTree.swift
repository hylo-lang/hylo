import FrontEnd

/// A hierarchical composition of access stacks, where each node tracks accesses
/// associated with an element.
public struct AccessStackTree<Element: Equatable> {

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
  /// Given a path `p`, `p[i]` identifies the element reached from the node
  /// denoted by `p[..<i]`. The empty path `[]` denotes the root element.
  ///
  /// For example, for a tree rooted at `A` with children `{B, C}`, where
  /// `C` has a child `{D}`:
  /// - `[B]` denotes `B`,
  /// - `[C, D]` denotes `D`, and
  /// - `[]` denotes `A`.
  public typealias Path = [Element]

  /// Adds an access of kind `a` derived from `p` at `path`, creating missing nodes as needed
  /// and invalidating conflicting accesses in overlapping nodes if necessary.
  public mutating func add(_ a: AccessKind, at path: Path, derivedFrom p: Access?)
    throws -> Access
  {
    precondition(!path.isEmpty)
    UNIMPLEMENTED()
  }

  /// Ends `a` at `path`.
  public mutating func end(_ a: Access, at path: Path) throws {
    precondition(!path.isEmpty)
    UNIMPLEMENTED()
  }

  /// Requires that the access `a` is valid for use at `path`,
  /// invalidating conflicting accesses in overlapping nodes if necessary.
  public mutating func require(_ a: Access, at path: Path) throws {
    precondition(!path.isEmpty)
    UNIMPLEMENTED()
  }

}
