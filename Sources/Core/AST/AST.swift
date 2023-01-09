import Utils
import ValModule

/// An abstract syntax tree.
public struct AST: Codable {

  /// The nodes in `self`.
  private var nodes: [AnyNode] = [AnyNode(BuiltinDecl())]

  /// The indices of the modules.
  ///
  /// - Invariant: All referred modules have a different name.
  public private(set) var modules: [NodeID<ModuleDecl>] = []

  /// The ID of the module containing Val's core library, if any.
  public var corelib: NodeID<ModuleDecl>?

  /// Creates an empty AST.
  public init() {}

  /// The ID of the node representing all built-in declarations.
  public var builtinDecl: NodeID<BuiltinDecl> { NodeID(rawValue: 0) }

  /// Inserts `n` into `self`, updating `diagnostics` if `n` is ill-formed.
  public mutating func insert<T: Node>(_ n: T, diagnostics: inout Diagnostics) -> NodeID<T> {
    n.validateForm(in: self, into: &diagnostics)

    let i = NodeID<T>(rawValue: nodes.count)
    if let n = n as? ModuleDecl {
      precondition(!modules.contains(where: { self[$0].name == n.name }), "duplicate module")
      modules.append(i as! NodeID<ModuleDecl>)
    }
    nodes.append(AnyNode(n))
    return i
  }

  /// Inserts `n` into `self`.
  ///
  /// - Precondition: `n` is well formed.
  public mutating func insert<T: Node>(synthesized n: T) -> NodeID<T> {
    var d = Diagnostics()
    let r = insert(n, diagnostics: &d)
    precondition(d.log.isEmpty, "ill-formed synthesized node \(n)")
    return r
  }

  // MARK: Node access

  /// Accesses the node at `position`.
  public subscript<T: Node>(position: NodeID<T>) -> T {
    get { nodes[position.rawValue].node as! T }
    _modify {
      var n = nodes[position.rawValue].node as! T
      defer { nodes[position.rawValue] = AnyNode(n) }
      yield &n
    }
  }

  /// Accesses the node at `position`.
  public subscript<T: Node>(position: NodeID<T>?) -> T? {
    position.map({ nodes[$0.rawValue].node as! T })
  }

  /// Accesses the node at `position`.
  public subscript<T: NodeIDProtocol>(position: T) -> Node {
    nodes[position.rawValue].node
  }

  /// Accesses the node at `position`.
  public subscript<T: NodeIDProtocol>(position: T?) -> Node? {
    position.map({ nodes[$0.rawValue].node })
  }

  /// Accesses the node at `position`.
  subscript(raw position: NodeID.RawValue) -> Node {
    nodes[position].node
  }

  /// Applies `transform` to the node at `position`.
  mutating func modify<T: Node>(at position: NodeID<T>, _ transform: (T) -> T) {
    let newNode = transform(self[position])

    var diagnostics = Diagnostics()
    newNode.validateForm(in: self, into: &diagnostics)
    assert(diagnostics.log.isEmpty, "\(diagnostics)")

    nodes[position.rawValue] = AnyNode(newNode)
  }

  // MARK: Core library

  /// Indicates whether the Core library has been loaded.
  public var isCoreModuleLoaded: Bool { corelib != nil }

  /// Returns the type named `name` defined in the core library or `nil` it does not exist.
  ///
  /// - Requires: The Core library must have been loaded.
  public func coreType(named name: String) -> ProductType? {
    precondition(isCoreModuleLoaded, "Core library is not loaded")

    for id in topLevelDecls(corelib!) where id.kind == ProductTypeDecl.self {
      let id = NodeID<ProductTypeDecl>(id)!
      if self[id].name == name {
        return ProductType(id, ast: self)
      }
    }

    return nil
  }

  /// Returns the trait named `name` defined in the core library or `nil` if it doesn not exist.
  ///
  /// - Requires: The core library must be loaded and assigned to `self.corelib`.
  public func coreTrait(named name: String) -> TraitType? {
    precondition(isCoreModuleLoaded, "Core library is not loaded")

    for id in topLevelDecls(corelib!) where id.kind == TraitDecl.self {
      let id = NodeID<TraitDecl>(rawValue: id.rawValue)
      if self[id].name == name {
        return TraitType(id, ast: self)
      }
    }

    return nil
  }

  // MARK: Helpers

  /// A collection that presents the top-level declarations of a module.
  public typealias TopLevelDecls = LazySequence<
    FlattenSequence<
      LazyMapSequence<
        LazySequence<[NodeID<TopLevelDeclSet>]>.Elements,
        [AnyDeclID]
      >.Elements
    >
  >

  /// Returns the IDs of the top-level declarations in the lexical scope of `module`.
  public func topLevelDecls(_ module: NodeID<ModuleDecl>) -> TopLevelDecls {
    let a = self[module].sources.lazy
      .map({ self[$0].decls })
      .joined()
    return a
  }

  /// Returns the IDs of the named patterns contained in `pattern`.
  public func names<T: PatternID>(in pattern: T) -> [(path: [Int], pattern: NodeID<NamePattern>)] {
    func visit(
      pattern: AnyPatternID,
      path: [Int],
      result: inout [(path: [Int], pattern: NodeID<NamePattern>)]
    ) {
      switch pattern.kind {
      case BindingPattern.self:
        let p = NodeID<BindingPattern>(rawValue: pattern.rawValue)
        visit(pattern: self[p].subpattern, path: path, result: &result)

      case ExprPattern.self:
        break

      case NamePattern.self:
        let p = NodeID<NamePattern>(rawValue: pattern.rawValue)
        result.append((path: path, pattern: p))

      case TuplePattern.self:
        let p = NodeID<TuplePattern>(rawValue: pattern.rawValue)
        for i in 0 ..< self[p].elements.count {
          visit(
            pattern: self[p].elements[i].pattern,
            path: path + [i],
            result: &result)
        }

      case WildcardPattern.self:
        break

      default:
        unreachable("unexpected pattern")
      }
    }

    var result: [(path: [Int], pattern: NodeID<NamePattern>)] = []
    visit(pattern: AnyPatternID(pattern), path: [], result: &result)
    return result
  }

  /// Returns the source origin of `expr`, if any.
  public func origin(of expr: FoldedSequenceExpr) -> SourceRange? {
    switch expr {
    case .leaf(let i):
      return self[i].origin

    case .infix(_, let lhs, let rhs):
      if let lhsRange = origin(of: lhs), let rhsRange = origin(of: rhs) {
        return lhsRange.extended(upTo: rhsRange.upperBound)
      } else {
        return nil
      }
    }
  }

}
