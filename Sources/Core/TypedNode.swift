// Renaming dance

public typealias ValNode = Node

extension AST {

  public typealias Node = ValNode

}

/// A projection from a `TypedProgram` of an AST node along with all the non-syntax information
/// related to that node.
@dynamicMemberLookup
public struct TypedNode<ID: NodeIDProtocol>: Hashable {

  /// The program program of which this node is a notional part.
  private let program: TypedProgram

  /// The node's identity in `program.ast`.
  public let id: ID

  public init(_ id: ID, in program: TypedProgram) {
    self.program = program
    self.id = id
  }

  /// The site from which self was parsed.
  public var site: SourceRange {
    program.ast[id].site
  }

  /// Equality comparison; only check the node ID.
  public static func == (lhs: TypedNode<ID>, rhs: TypedNode<ID>) -> Bool {
    lhs.id == rhs.id
  }

  /// Hashes the value of `id` into `hasher`.
  public func hash(into hasher: inout Hasher) {
    hasher.combine(id)
  }

}

extension AST.Node {

  /// Type describing the current node with type information.
  public typealias Typed = TypedNode<ID>

}

extension TypedProgram {

  // Used for renaming; we want to use `TypedNode` typealias inside NodeIDProtocol
  public typealias NodeWithTypeInfo<ID: NodeIDProtocol> = TypedNode<ID>

}

extension NodeIDProtocol {

  /// Node with type information, corresponding to this node ID
  public typealias TypedNode = TypedProgram.NodeWithTypeInfo<Self>

}

extension TypedProgram {

  /// Bundles `id` together with `self`.
  public subscript<TargetID: NodeIDProtocol>(_ id: TargetID) -> TypedNode<TargetID> {
    TypedNode(id, in: self)
  }

}

extension TypedNode where ID: ConcreteNodeID {

  /// The corresponding AST node.
  private var syntax: ID.Subject {
    program.ast[id]
  }

  /// Accesses the given member of the corresponding AST node.
  public subscript<Target>(dynamicMember m: KeyPath<ID.Subject, Target>) -> Target {
    syntax[keyPath: m]
  }

  /// Accesses the given member of the corresponding AST node as a corresponding
  /// `TypedNode`
  public subscript<TargetID: NodeIDProtocol>(
    dynamicMember m: KeyPath<ID.Subject, TargetID>
  ) -> TypedNode<TargetID> {
    .init(syntax[keyPath: m], in: program)
  }

  /// Accesses the given member of the corresponding AST node as a corresponding lazy collection
  /// of `TypedNode`s.
  public subscript<TargetID: NodeIDProtocol>(
    dynamicMember m: KeyPath<ID.Subject, [TargetID]>
  ) -> LazyMapCollection<[TargetID], TypedNode<TargetID>> {
    syntax[keyPath: m].lazy.map { .init($0, in: program) }
  }

  /// Accesses the given member of the corresponding AST node as a corresponding `TypedNode?`
  public subscript<TargetID: NodeIDProtocol>(
    dynamicMember m: KeyPath<ID.Subject, TargetID?>
  ) -> TypedNode<TargetID>? {
    syntax[keyPath: m].map { .init($0, in: program) }
  }

  /// Creates an instance denoting the same node as `s`, or fails if `s` does not refer to a
  /// `Target` node.
  public init?<SourceID, Target>(_ s: TypedNode<SourceID>) where ID == NodeID<Target> {
    guard let myID = ID(s.id) else { return nil }
    program = s.program
    id = myID
  }

}

extension TypedNode {

  /// The corresponding node kind.
  public var kind: NodeKind { id.kind }

}

extension TypedNode where ID: ScopeID {

  /// The parent scope, if any
  public var parent: AnyScopeID.TypedNode? {
    program.scopeToParent[id].map { .init($0, in: program) }
  }

  /// The declarations in this immediate scope.
  public var decls: LazyMapCollection<[AnyDeclID], AnyDeclID.TypedNode> {
    program.scopeToDecls[id, default: []].lazy.map { .init($0, in: program) }
  }

  /// The module containing this scope.
  public var module: ModuleDecl.Typed {
    .init(program.module(containing: id), in: program)
  }

}

extension TypedNode where ID: DeclID {

  /// The scope in which this declaration resides.
  public var scope: AnyScopeID.TypedNode {
    .init(program.declToScope[id]!, in: program)
  }

  /// The type of the declared entity.
  /// If the declaration type is invalid, we return `.error` type.
  public var type: AnyType {
    program.declTypes[id] ?? .error
  }

  /// The implicit captures for the declared entity.
  public var implicitCaptures: [ImplicitCapture]? {
    program.implicitCaptures[id]
  }

}

extension TypedNode where ID == VarDecl.ID {

  /// The binding decl containing this var.
  public var binding: BindingDecl.Typed {
    .init(program.varToBinding[id]!, in: program)
  }

}

extension TypedNode where ID: ExprID {

  /// The type of this expression
  public var type: AnyType {
    program.exprTypes[id]!
  }

}

extension TypedNode where ID == NameExpr.ID {

  public enum Domain: Equatable {

    /// No domain.
    case none

    /// Domain is implicit; the expression denotes a type member.
    case implicit

    /// Domain is a value expression or a type identifier.
    case expr(AnyExprID.TypedNode)

  }

  /// The domain of the name, if it is qualified.
  public var domain: Domain {
    switch syntax.domain {
    case .none:
      return .none
    case .implicit:
      return .implicit
    case .expr(let expr):
      return .expr(program[expr])
    }
  }

  /// The domain expression node, if the domain is an expression
  public var domainExpr: AnyExprID.TypedNode? {
    switch syntax.domain {
    case .expr(let expr):
      return program[expr]
    default:
      return nil
    }
  }

  /// A reference to a declaration.
  public enum DeclRef: Hashable {

    /// A direct reference.
    case direct(AnyDeclID.TypedNode)

    /// A reference to a member declaration bound to `self`.
    case member(AnyDeclID.TypedNode)

    /// A reference to a built-in function.
    case builtinFunction(BuiltinFunction)

    /// A reference to a built-in type.
    case builtinType

    /// Accesses the referred declaration  if `self` is `.direct` or `.member`.
    public var decl: AnyDeclID.TypedNode? {
      switch self {
      case .direct(let d):
        return d
      case .member(let d):
        return d
      default:
        return nil
      }
    }

  }

  /// The declaration of this name.
  public var decl: DeclRef {
    switch program.referredDecls[id]! {
    case .direct(let d):
      return .direct(program[d])
    case .member(let d):
      return .member(program[d])
    case .builtinFunction(let f):
      return .builtinFunction(f)
    case .builtinType:
      return .builtinType
    }
  }

}

extension TypedNode where ID: PatternID {

  /// The names associated with this pattern.
  public var names: [(path: PartPath, pattern: NamePattern.Typed)] {
    program.ast.names(in: id).map({ (path: $0.path, pattern: program[$0.pattern]) })
  }

}

extension TypedNode where ID == ModuleDecl.ID {

  /// The top-level declarations in the module.
  public var topLevelDecls: some Collection<TypedNode<AnyDeclID>> {
    program.ast.topLevelDecls(id).map({ program[$0] })
  }

}

extension TypedNode where ID == FunctionDecl.ID {

  /// The body of the function, containing typed information
  public enum Body {

    /// An expression body.
    case expr(AnyExprID.TypedNode)

    /// A block body.
    case block(BraceStmt.Typed)

  }

  /// The body of the declaration, if any (in typed formed).
  public var body: Body? {
    switch syntax.body {
    case .expr(let expr):
      return .expr(program[expr])

    case .block(let stmt):
      return .block(program[stmt])

    case .none:
      return .none
    }
  }

  /// The parameters of the function.
  public var parameters: [ParameterDecl.Typed] {
    return syntax.parameters.lazy.map({
      return program[$0]
    })
  }

}

extension TypedNode where ID == SequenceExpr.ID {

  /// A map from (typed) sequence expressions to their evaluation order.
  public var foldedSequenceExprs: FoldedSequenceExpr? {
    program.foldedSequenceExprs[id]
  }

}

extension TypedNode where ID == AnyNodeID {

  /// Any typed node is convertible to TypedNode<AnyNodeID>.
  public init<SourceID: NodeIDProtocol>(_ s: TypedNode<SourceID>) {
    program = s.program
    id = AnyNodeID(s.id)
  }

}

extension TypedNode where ID == AnyExprID {

  /// Any typed expression node is convertible to TypedNode<AnyExprID>.
  public init<SourceID: ExprID>(_ s: TypedNode<SourceID>) {
    program = s.program
    id = AnyExprID(s.id)
  }

}
