import FrontEnd

/// A hierarchical composition of access stacks, where each node tracks accesses
/// for a region in an `Allocation`.
public struct AccessStackTree<T: Equatable> {

  /// Type of index of a node in `nodes`.
  private typealias Index = Int

  /// A tree node representing a region and its associated accesses.
  private struct Node {

    /// Identity of the region represented by node.
    public let id: T

    /// Accesses currently active on this node.
    public var accesses: [Access]

    /// The indices of this node children.
    public var children: [Index]

    /// Node with no accesses and children.
    public init(_ id: T) {
      self.id = id
      accesses = []
      children = []
    }

  }

  /// Contiguous storage of all nodes in the tree.
  private var nodes: [Node] = []

  /// Position of nodes that has been freed and is available to re-use.
  private var freeNodes: [Index] = []

  /// Position of root node.
  private var root: Index

  public init(of allocation: T) {
    nodes.append(Node(allocation))
    root = 0
  }

  /// Adds access of kind `a` derived from `p` to `path.last!` element.
  public mutating func add(_ a: AccessKind, at path: [T], derivedFrom p: Access?) throws
    -> Access
  {
    UNIMPLEMENTED()
  }

  /// Ends `a` on given path.
  public mutating func end(_ a: Access, at path: [T]) throws {
    UNIMPLEMENTED()
  }

  /// Throws iff can't grant to use `a`.
  public mutating func require(_ a: Access, at path: [T]) throws {
    UNIMPLEMENTED()
  }

}
