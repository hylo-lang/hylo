import Foundation
import Utils

/// An abstract syntax tree.
public struct AST {

  /// The stored representation of an AST; distinguished for encoding/decoding purposes.
  private struct Storage: Codable {

    /// The nodes in `self`.
    public var nodes: [AnyNode] = []

    /// The indices of the modules in the AST.
    ///
    /// Indices are ordered by module dependency. If the module identified by the index at position
    /// `i` depends on the one identified by the index at position `j`, then `i` precedes `j`.
    ///
    /// - Invariant: All referred modules have a different name.
    public var modules: [ModuleDecl.ID] = []

    /// The ID of the module containing Val's core library, if any.
    public var coreLibrary: ModuleDecl.ID?

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
  public private(set) var modules: [ModuleDecl.ID] {
    get { storage.modules }
    set { storage.modules = newValue }
    _modify { yield &storage.modules }
  }

  /// The ID of the module containing Val's core library, if any.
  public var coreLibrary: ModuleDecl.ID? {
    get { storage.coreLibrary }
    set { storage.coreLibrary = newValue }
    _modify { yield &storage.coreLibrary }
  }

  /// Creates an empty AST.
  public init() {}

  /// Inserts `n` into `self`, updating `diagnostics` if `n` is ill-formed.
  public mutating func insert<T: Node>(_ n: T, diagnostics: inout DiagnosticSet) -> T.ID {
    n.validateForm(in: self, into: &diagnostics)

    let i = T.ID(rawValue: nodes.count)
    if let n = n as? ModuleDecl {
      precondition(
        !modules.contains(where: { self[$0].baseName == n.baseName }), "duplicate module")
      modules.append(i as! ModuleDecl.ID)
    }
    nodes.append(AnyNode(n))
    return i
  }

  /// Inserts `n` into `self`.
  ///
  /// - Precondition: `n` is well formed.
  public mutating func insert<T: Node>(synthesized n: T) -> T.ID {
    var d = DiagnosticSet()
    let r = insert(n, diagnostics: &d)
    precondition(d.elements.isEmpty, "ill-formed synthesized node \(n)\n\(d)")
    return r
  }

  // MARK: Node access

  /// Accesses the node at `position`.
  public subscript<T: ConcreteNodeID>(position: T) -> T.Subject {
    nodes[position.rawValue].node as! T.Subject
  }

  /// Accesses the node at `position`.
  public subscript<T: ConcreteNodeID>(position: T?) -> T.Subject? {
    position.map({ nodes[$0.rawValue].node as! T.Subject })
  }

  /// Accesses the node at `position`.
  public subscript<T: NodeIDProtocol>(position: T) -> Node {
    nodes[position.rawValue].node
  }

  /// Accesses the node at `position`.
  public subscript<T: NodeIDProtocol>(position: T?) -> Node? {
    position.map({ nodes[$0.rawValue].node })
  }

  /// A sequence of concrete nodes projected from an AST.
  public typealias ConcreteProjectedSequence<T> = LazyMapSequence<
    LazySequence<T>.Elements,
    T.Element.Subject
  > where T: Sequence, T.Element: ConcreteNodeID

  /// Projects a sequence containing the nodes at `positions`.
  public subscript<T: Sequence>(positions: T) -> ConcreteProjectedSequence<T> {
    positions.lazy.map({ (n) in self[n] })
  }

  // MARK: Core library

  /// Indicates whether the Core library has been loaded.
  public var isCoreModuleLoaded: Bool { coreLibrary != nil }

  /// Returns the type named `name` defined in the core library or `nil` it does not exist.
  ///
  /// - Requires: The Core library must have been loaded.
  public func coreType(_ name: String) -> ProductType? {
    precondition(isCoreModuleLoaded, "Core library is not loaded")

    for id in topLevelDecls(coreLibrary!) where id.kind == ProductTypeDecl.self {
      let id = ProductTypeDecl.ID(id)!
      if self[id].baseName == name {
        return ProductType(id, ast: self)
      }
    }

    return nil
  }

  /// Returns the trait named `name` defined in the core library or `nil` if it does not exist.
  ///
  /// - Requires: The Core library must have been loaded.
  public func coreTrait(_ name: String) -> TraitType? {
    precondition(isCoreModuleLoaded, "Core library is not loaded")

    for id in topLevelDecls(coreLibrary!) where id.kind == TraitDecl.self {
      let id = TraitDecl.ID(id)!
      if self[id].baseName == name {
        return TraitType(id, ast: self)
      }
    }

    return nil
  }

  /// Returns the trait describing types whose instances are expressible by this literal or `nil`
  /// if it does not exist.
  ///
  /// - Requires: The Core library must have been loaded.
  public func coreTrait<T: Expr>(forTypesExpressibleBy literal: T.Type) -> TraitType? {
    switch literal.kind {
    case FloatLiteralExpr.self:
      return coreTrait("ExpressibleByFloatLiteral")
    case IntegerLiteralExpr.self:
      return coreTrait("ExpressibleByIntegerLiteral")
    default:
      return nil
    }
  }

  /// `Val.Deinitializable` trait from the Core library.
  ///
  /// - Requires: The Core library must have been loaded.
  public var deinitializableTrait: TraitType { coreTrait("Deinitializable")! }

  /// `Val.Movable` trait from the Core library.
  ///
  /// - Requires: The Core library must have been loaded.
  public var movableTrait: TraitType { coreTrait("Movable")! }

  /// `Val.Copyable` trait from the Core library.
  ///
  /// - Requires: The Core library must have been loaded.
  public var copyableTrait: TraitType { coreTrait("Copyable")! }

  // MARK: Helpers

  /// Returns the IDs of the top-level declarations in the lexical scope of `module`.
  public func topLevelDecls(_ module: ModuleDecl.ID) -> some Collection<AnyDeclID> {
    self[self[module].sources].map(\.decls).joined()
  }

  /// Returns the requirements named `n` in `t`.
  ///
  /// If `n` is overloaded, the requirements are returned in the order in which they are declared
  /// in the source code.
  public func requirements(_ n: Name, in t: TraitDecl.ID) -> [AnyDeclID] {
    self[t].members.compactMap({ (m) -> AnyDeclID? in
      switch m.kind {
      case AssociatedValueDecl.self:
        return (n == Name(stem: self[AssociatedValueDecl.ID(m)!].baseName)) ? m : nil

      case AssociatedTypeDecl.self:
        return (n == Name(stem: self[AssociatedTypeDecl.ID(m)!].baseName)) ? m : nil

      case FunctionDecl.self:
        return (n == Name(of: FunctionDecl.ID(m)!, in: self)) ? m : nil

      case InitializerDecl.self:
        return (n == Name(of: InitializerDecl.ID(m)!, in: self)) ? m : nil

      case MethodDecl.self:
        let d = MethodDecl.ID(m)!
        if let i = n.introducer {
          guard n.removingIntroducer() == Name(of: d, in: self) else { return nil }
          return self[d].impls
            .first(where: { self[$0].introducer.value == i })
            .map(AnyDeclID.init(_:))
        } else {
          return (Name(of: d, in: self) == n) ? m : nil
        }

      case SubscriptDecl.self:
        let d = SubscriptDecl.ID(m)!
        if let i = n.introducer {
          guard n.removingIntroducer() == Name(of: d, in: self) else { return nil }
          return self[d].impls
            .first(where: { self[$0].introducer.value == i })
            .map(AnyDeclID.init(_:))
        } else {
          return (Name(of: d, in: self) == n) ? m : nil
        }

      default:
        return nil
      }
    })
  }

  /// Returns the declaration of `Deinitializable.deinit`.
  public func deinitRequirement() -> FunctionDecl.ID {
    let d = requirements(Name(stem: "deinit"), in: deinitializableTrait.decl)
    return FunctionDecl.ID(d[0])!
  }

  /// Returns the declaration of `Movable.take_value`'s requirement for variant `access`.
  ///
  /// Use the `.set` or `.inout` access in order to get the declaration of the move-initialization
  /// or move-assignment, respectively.
  ///
  /// - Requires: `access` is either `.set` or `.inout`.
  public func moveRequirement(_ access: AccessEffect) -> MethodImpl.ID {
    let d = requirements(
      Name(stem: "take_value", labels: ["from"], introducer: access),
      in: movableTrait.decl)
    return MethodImpl.ID(d[0])!
  }

  /// Returns a table mapping each parameter of `d` to its default argument if `d` is a function,
  /// initializer, method or subscript declaration. Otherwise, returns `nil`.
  public func defaultArguments(of d: AnyDeclID) -> [AnyExprID?]? {
    let parameters: [ParameterDecl.ID]
    switch d.kind {
    case FunctionDecl.self:
      parameters = self[FunctionDecl.ID(d)!].parameters
    case InitializerDecl.self:
      parameters = self[InitializerDecl.ID(d)!].parameters
    case MethodDecl.self:
      parameters = self[MethodDecl.ID(d)!].parameters
    case SubscriptDecl.self:
      parameters = self[SubscriptDecl.ID(d)!].parameters ?? []
    default:
      return nil
    }
    return self[parameters].map(\.defaultValue)
  }

  /// Returns the subfields and pattern IDs of the named patterns contained in `p`.
  public func names<T: PatternID>(in p: T) -> [(subfield: RecordPath, pattern: NamePattern.ID)] {
    func visit(
      pattern: AnyPatternID,
      subfield: RecordPath,
      result: inout [(subfield: RecordPath, pattern: NamePattern.ID)]
    ) {
      switch pattern.kind {
      case BindingPattern.self:
        visit(
          pattern: self[BindingPattern.ID(pattern)!].subpattern, subfield: subfield, result: &result
        )

      case ExprPattern.self:
        break

      case NamePattern.self:
        result.append((subfield: subfield, pattern: NamePattern.ID(pattern)!))

      case TuplePattern.self:
        let x = TuplePattern.ID(pattern)!
        for i in 0 ..< self[x].elements.count {
          visit(pattern: self[x].elements[i].pattern, subfield: subfield + [i], result: &result)
        }

      case WildcardPattern.self:
        break

      default:
        unexpected(pattern, in: self)
      }
    }

    var result: [(subfield: RecordPath, pattern: NamePattern.ID)] = []
    visit(pattern: AnyPatternID(p), subfield: [], result: &result)
    return result
  }

  /// Calls `action` on each sub-pattern in `pattern` and its corresponding sub-expression in
  /// `expression`, along with the path to this sub-pattern, relative to `root`.
  ///
  /// Use this method to walk a pattern and a corresponding expression side by side and perform an
  /// action for each pair. Children of tuple patterns are visited in pre-order if and only ifs the
  /// corresponding expression is also a tuple. Otherwise, `action` is called on the tuple and the
  /// sub-patterns are not visited.
  ///
  /// - Requires: `expression` structurally matches `pattern`: If `pattern` and `expression` are
  ///   tuples, then they have equal lengths and labels.
  public func walking(
    pattern: AnyPatternID, expression: AnyExprID,
    at root: RecordPath = [],
    _ action: (_ subfield: RecordPath, _ subpattern: AnyPatternID, _ subexpression: AnyExprID) ->
      Void
  ) {
    switch pattern.kind {
    case BindingPattern.self:
      let p = self[BindingPattern.ID(pattern)]!.subpattern
      walking(pattern: p, expression: expression, at: root, action)

    case TuplePattern.kind:
      guard let e = TupleExpr.ID(expression) else {
        walking(pattern: pattern, expression: expression, at: root, action)
        return
      }

      let p = TuplePattern.ID(pattern)!
      for i in self[p].elements.indices {
        let a = self[p].elements[i]
        let b = self[e].elements[i]
        walking(pattern: a.pattern, expression: b.value, at: root + [i], action)
      }

    default:
      action(root, pattern, expression)
    }
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
