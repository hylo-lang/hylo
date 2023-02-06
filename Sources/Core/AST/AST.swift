import Foundation
import Utils
import ValModule

/// An abstract syntax tree.
public struct AST {

  /// The stored representation of an AST; distinguished for encoding/decoding purposes.
  private struct Storage: Codable {
    /// The nodes in `self`.
    public var nodes: [AnyNode] = []

    /// The indices of the modules.
    ///
    /// - Invariant: All referred modules have a different name.
    public var modules: [NodeID<ModuleDecl>] = []

    /// The ID of the module containing Val's core library, if any.
    public var corelib: NodeID<ModuleDecl>?
  }

  /// The notional stored properties of `self`; distinguished for encoding/decoding purposes.
  private var storage = Storage()

  /// The nodes in `self`.
  private var nodes: [AnyNode] {
    get { storage.nodes }
    set { storage.nodes = newValue }
    _modify { yield &storage.nodes }
  }

  /// The indices of the modules.
  ///
  /// - Invariant: All referred modules have a different name.
  public private(set) var modules: [NodeID<ModuleDecl>] {
    get { storage.modules }
    set { storage.modules = newValue }
    _modify { yield &storage.modules }
  }

  /// The ID of the module containing Val's core library, if any.
  public var corelib: NodeID<ModuleDecl>? {
    get { storage.corelib }
    set { storage.corelib = newValue }
    _modify { yield &storage.corelib }
  }

  /// Creates an empty AST.
  public init() {}

  /// Inserts `n` into `self`, updating `diagnostics` if `n` is ill-formed.
  public mutating func insert<T: Node>(_ n: T, diagnostics: inout Diagnostics) -> NodeID<T> {
    n.validateForm(in: self, into: &diagnostics)

    let i = NodeID<T>(rawValue: nodes.count)
    if let n = n as? ModuleDecl {
      precondition(
        !modules.contains(where: { self[$0].baseName == n.baseName }), "duplicate module")
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
      if self[id].baseName == name {
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
      let id = NodeID<TraitDecl>(id)!
      if self[id].baseName == name {
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
        let p = NodeID<BindingPattern>(pattern)!
        visit(pattern: self[p].subpattern, path: path, result: &result)

      case ExprPattern.self:
        break

      case NamePattern.self:
        let p = NodeID<NamePattern>(pattern)!
        result.append((path: path, pattern: p))

      case TuplePattern.self:
        let p = NodeID<TuplePattern>(pattern)!
        for i in 0 ..< self[p].elements.count {
          visit(
            pattern: self[p].elements[i].pattern,
            path: path + [i],
            result: &result)
        }

      case WildcardPattern.self:
        break

      default:
        unexpected("pattern", found: pattern, of: self)
      }
    }

    var result: [(path: [Int], pattern: NodeID<NamePattern>)] = []
    visit(pattern: AnyPatternID(pattern), path: [], result: &result)
    return result
  }

  /// Returns the source site of `expr`
  public func site(of expr: FoldedSequenceExpr) -> SourceRange {
    switch expr {
    case .leaf(let i):
      return self[i].site

    case .infix(_, let lhs, let rhs):
      let lhsSite = site(of: lhs)
      let rhsSite = site(of: rhs)
      return lhsSite.extended(upTo: rhsSite.end)
    }
  }

}

extension AST: Codable {

  /// The format of data that must be injected into a `StatefulEncoder` for serializing an `AST`
  /// instance.
  typealias EncodingState = SourceFile.EncodingState

  /// The format of data that must be injected into a `StatefulDecoder` for deserializing an `AST`
  /// instance.
  typealias DecodingState = SourceFile.DecodingState

  /// The parts of an encoded AST.
  private enum CodingKeys: String, CodingKey {

    /// The main content of the AST
    case storage

    /// Separately encoded content such as `SourceFile` representations, used for reconstructing
    /// `storage`.
    case decodingState

  }

  public init(from decoder: Decoder) throws {
    let container = try decoder.container(keyedBy: CodingKeys.self)

    let decodingContext = try container.decode(DecodingState.self, forKey: .decodingState)
    decoder[state: DecodingState.self] = decodingContext
    self.storage = try container.decode(AST.Storage.self, forKey: .storage)
  }

  public func encode(to encoder: Encoder) throws {
    var container = encoder.container(keyedBy: CodingKeys.self)

    try container.encode(storage, forKey: .storage)
    let decodingState = encoder[state: EncodingState.self].decodingState()
    try container.encode(decodingState, forKey: .decodingState)

  }

}

extension StatefulEncoder {

  /// `self` prepared for encoding an `AST`.
  public var forAST: Self {
    var r = self
    r.setState(AST.EncodingState())
    return r
  }

}

extension StatefulDecoder {

  /// `self` prepared for decoding an `AST`.
  public var forAST: Self {
    var r = self
    r.setState(AST.DecodingState())
    return r
  }

}
