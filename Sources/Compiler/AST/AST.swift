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

  /// The source range of each node.
  public internal(set) var ranges = ASTProperty<SourceRange>()

  /// Creates an empty AST.
  public init() {}

  /// The ID of the node representing all built-in declarations.
  public var builtinDecl: NodeID<BuiltinDecl> { NodeID(rawValue: 0) }

  /// Returns the scope hierarchy.
  func scopeHierarchy() -> ScopeHierarchy {
    var builder = ScopeHierarchyBuilder(ast: self)
    return builder.build()
  }

  /// Inserts `n` into `self`.
  public mutating func insert<T: Node>(_ n: T) -> NodeID<T> {
    let i = NodeID<T>(rawValue: nodes.count)
    if let n = n as? ModuleDecl {
      precondition(!modules.contains(where: { self[$0].name == n.name }), "duplicate module")
      modules.append(i as! NodeID<ModuleDecl>)
    }
    nodes.append(AnyNode(n))
    return i
  }

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

  // MARK: Core library

  /// Imports the core library into `self`.
  ///
  /// - Requires: The core library must not have been already imported.
  public mutating func importCoreModule() throws {
    if corelib != nil { throw CompilerError(description: "module already loaded") }
    corelib = insert(ModuleDecl(name: "Val"))

    try withFiles(in: ValModule.core!, { (sourceURL) in
      if sourceURL.pathExtension != "val" { return true }

      let sourceFile = try SourceFile(contentsOf: sourceURL)
      let (decls, diagnostics) = Parser.parse(sourceFile, into: corelib!, in: &self)
      if (decls == nil) || !diagnostics.isEmpty {
        throw CompilerError(description: "parser failed", diagnostics: diagnostics)
      }
      return true
    })
  }

  /// Returns the type named `name` defined in the core library or `nil` it does not exist.
  ///
  /// - Requires: The core library must be loaded and assigned to `self.corelib`.
  public func coreType(named name: String) -> ProductType? {
    let corelib = corelib ?? preconditionFailure("core library is not loaded")

    for id in topLevelDecls(corelib) where id.kind == .productTypeDecl {
      let id = NodeID<ProductTypeDecl>(id)!
      if self[id].name == name {
        return ProductType(decl: id, ast: self)
      }
    }

    return nil
  }

  /// Returns the trait named `name` defined in the core library or `nil` if it doesn not exist.
  ///
  /// - Requires: The core library must be loaded and assigned to `self.corelib`.
  public func coreTrait(named name: String) -> TraitType? {
    let corelib = corelib ?? preconditionFailure("core library is not loaded")

    for id in topLevelDecls(corelib) where id.kind == .traitDecl {
      let id = NodeID<TraitDecl>(rawValue: id.rawValue)
      if self[id].name == name {
        return TraitType(decl: id, ast: self)
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
      >.Elements>>

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
      case .bindingPattern:
        let p = NodeID<BindingPattern>(rawValue: pattern.rawValue)
        visit(pattern: self[p].subpattern, path: path, result: &result)

      case .exprPattern:
        break

      case .namePattern:
        let p = NodeID<NamePattern>(rawValue: pattern.rawValue)
        result.append((path: path, pattern: p))

      case .tuplePattern:
        let p = NodeID<TuplePattern>(rawValue: pattern.rawValue)
        for i in 0 ..< self[p].elements.count {
          visit(
            pattern: self[p].elements[i].pattern,
            path: path + [i],
            result: &result)
        }

      case .wildcardPattern:
        break

      default:
        unreachable("unexpected pattern")
      }
    }

    var result: [(path: [Int], pattern: NodeID<NamePattern>)] = []
    visit(pattern: AnyPatternID(pattern), path: [], result: &result)
    return result
  }

  // MARK: Synthesis

  /// Retrieves or synthesizes the declaration of the memberwise initializer of `d`.
  mutating func memberwiseInitDecl(
    of d: NodeID<ProductTypeDecl>,
    updating scopeHierarchy: inout ScopeHierarchy
  ) -> NodeID<FunDecl> {
    // Look for the declaration.
    for member in self[d].members where member.kind == .funDecl {
      let m = NodeID<FunDecl>(rawValue: member.rawValue)
      if self[m].introducer.value == .memberwiseInit { return m }
    }

    // Synthesize the declaration.
    let m = insert(FunDecl(introducer: SourceRepresentable(value: .memberwiseInit)))
    self[d].members.insert(AnyDeclID(m), at: 0)
    scopeHierarchy.insert(decl: m, into: AnyScopeID(d))

    return m
  }

}
