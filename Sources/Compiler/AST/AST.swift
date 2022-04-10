/// A collection of module declarations representing an abstract syntax tree.
public struct AST {

  /// The nodes in the AST.
  ///
  /// - Notes: Should be an array with tombstones once .
  private var nodes: [Any]

  /// A collection with the indices of the modules of the AST.
  public private(set) var modules: [NodeIndex<ModuleDecl>]

  /// The source range annotations of the nodes.
  public var ranges: NodeMap<SourceRange>

  /// Creates an empty AST.
  public init() {
    nodes = []
    modules = []
    ranges = NodeMap()
  }

  /// The index of the module containing Val's standard library, if present in the AST.
  public var std: NodeIndex<ModuleDecl>?

  /// Returns the scope hierarchy of the AST.
  func scopeHierarchy() -> ScopeHierarchy {
    var builder = ScopeHierarchyBuilder()
    return builder.build(hierarchyOf: self)
  }

  /// Inserts a node in the AST.
  public mutating func insert<T: Node>(_ node: T) -> NodeIndex<T> {
    let i = NodeIndex<T>(rawValue: nodes.count)
    nodes.append(node)
    if node is ModuleDecl { modules.append(i as! NodeIndex<ModuleDecl>) }
    return i
  }

  /// Accesses the node at `position` for reading or writing.
  public subscript<T: Node>(position: NodeIndex<T>) -> T {
    _read { yield nodes[position.rawValue] as! T }
    _modify {
      var n = nodes[position.rawValue] as! T
      defer { nodes[position.rawValue] = n }
      yield &n
    }
  }

  /// Accesses the node at `position` for reading.
  public subscript<T: Node>(position: NodeIndex<T>?) -> T? {
    _read { yield position.map({ nodes[$0.rawValue] as! T }) }
  }

  /// Accesses the node at `position` for reading.
  public subscript<T: NodeIndexProtocol>(position: T) -> Node {
    _read { yield nodes[position.rawValue] as! Node }
  }

  /// Accesses the node at `position` for reading.
  public subscript<T: NodeIndexProtocol>(position: T?) -> Node? {
    _read { yield position.map({ nodes[$0.rawValue] as! Node }) }
  }

  /// Accesses the node at `position` for reading.
  subscript(raw position: NodeIndex.RawValue) -> Any {
    _read { yield nodes[position] }
  }

}
