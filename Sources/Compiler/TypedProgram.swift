/// A data structure representing a typed Val program ready to be lowered.
public struct TypedProgram: Program {

  public let ast: AST

  public let scopeToParent: ASTProperty<AnyScopeID>

  public let scopeToDecls: ASTProperty<[AnyDeclID]>

  public let declToScope: DeclProperty<AnyScopeID>

  public let varToBinding: [NodeID<VarDecl>: NodeID<BindingDecl>]

  /// The overarching type of each declaration.
  public let declTypes: DeclProperty<AnyType>

  /// The type of each expression.
  public let exprTypes: ExprProperty<AnyType>

  /// A map from function and subscript declarations to their implicit captures.
  public let implicitCaptures: DeclProperty<[ImplicitCapture]>

  /// A map from name expression to its referred declaration.
  public let referredDecls: [NodeID<NameExpr>: DeclRef]

  /// A map from sequence expressions to their evaluation order.
  public let foldedSequenceExprs: [NodeID<SequenceExpr>: FoldedSequenceExpr]

  /// Creates a typed program from a scoped program and property maps describing type annotations.
  public init(
    annotating program: ScopedProgram,
    declTypes: DeclProperty<AnyType>,
    exprTypes: ExprProperty <AnyType>,
    implicitCaptures: DeclProperty<[ImplicitCapture]>,
    referredDecls: [NodeID<NameExpr>: DeclRef],
    foldedSequenceExprs: [NodeID<SequenceExpr>: FoldedSequenceExpr]
  ) {
    self.ast = program.ast
    self.scopeToParent = program.scopeToParent
    self.scopeToDecls = program.scopeToDecls
    self.declToScope = program.declToScope
    self.varToBinding = program.varToBinding
    self.declTypes = declTypes
    self.exprTypes = exprTypes
    self.implicitCaptures = implicitCaptures
    self.referredDecls = referredDecls
    self.foldedSequenceExprs = foldedSequenceExprs
  }
}

// Renaming dance

public typealias ValNode = Node
extension AST {
  public typealias Node = ValNode
}

extension TypedProgram {
  /// A projection from a `TypedProgram` of an AST node along with all the non-syntax information
  /// related to that node.
  @dynamicMemberLookup
  public struct SomeNode<ID: NodeIDProtocol> : Hashable {
    /// The whole program of which this node is a notional part.
    fileprivate let whole: TypedProgram

    /// The node's identity in `whole.ast`.
    let id: ID

    public static func == (lhs: TypedProgram.SomeNode<ID>, rhs: TypedProgram.SomeNode<ID>) -> Bool {
      lhs.id == rhs.id
    }
    
    public func hash(into hasher: inout Hasher) {
      hasher.combine(id)
    }
  }

  typealias AnyScope = SomeNode<AnyScopeID>
  typealias AnyDecl = SomeNode<AnyDeclID>
  typealias AnyExpr = SomeNode<AnyExprID>
  typealias AnyNode = SomeNode<AnyNodeID>
  
  public typealias Node<T: AST.Node> = SomeNode<NodeID<T>>
  
  public subscript<TargetID: NodeIDProtocol>(_ id: TargetID) -> TypedProgram.SomeNode<TargetID>
  {
    SomeNode<TargetID>(whole: self, id: id)
  }
}

extension TypedProgram.SomeNode where ID: ConcreteNodeID {
  
  /// The corresponding AST node.
  private var syntax: ID.Subject {
    whole.ast[NodeID(id)!]
  }
  
  /// Accesses the given member of the corresponding AST node.
  subscript<Target>(dynamicMember m: KeyPath<ID.Subject, Target>) -> Target
  {
    syntax[keyPath: m]
  }
  
  /// Accesses the given member of the corresponding AST node as a corresponding
  /// `TypedProgram.SomeNode`
  subscript<TargetID: NodeIDProtocol>(
    dynamicMember m: KeyPath<ID.Subject, TargetID>
  ) -> TypedProgram.SomeNode<TargetID>
  {
    .init(whole: whole, id: syntax[keyPath: m])
  }
  
  /// Accesses the given member of the corresponding AST node as a corresponding lazy collection
  /// of `TypedProgram.SomeNode`s.
  subscript<TargetID: NodeIDProtocol>(
    dynamicMember m: KeyPath<ID.Subject, [TargetID]>
  ) -> LazyMapCollection<[TargetID], TypedProgram.SomeNode<TargetID>>
  {
    syntax[keyPath: m].lazy.map { .init(whole: whole, id: $0) }
  }
  
  /// Accesses the given member of the corresponding AST node as a corresponding
  /// `TypedProgram.SomeNode?`
  subscript<TargetID: NodeIDProtocol>(
    dynamicMember m: KeyPath<ID.Subject, TargetID?>
  ) -> TypedProgram.SomeNode<TargetID>?
  {
    syntax[keyPath: m].map { .init(whole: whole, id: $0) }
  }
  
  /// Creates an instance denoting the same node as `s`, or fails if `s` does not refer to a
  /// `Target` node.
  init?<SourceID/*, Target*/>(_ s: TypedProgram.SomeNode<SourceID>)
  //    where ID == NodeID<Target>
  {
    guard let myID = NodeID<ID.Subject>(s.id) else { return nil }
    whole = s.whole
    id = .init(myID)
  }
}

extension TypedProgram.SomeNode {
  
  /// The corresponding node kind.
  var kind: NodeKind { id.kind }
}

extension TypedProgram.SomeNode where ID: ScopeID {
  /// The parent scope, if any
  var parent: TypedProgram.AnyScope? {
    whole.scopeToParent[id].map { .init(whole: whole, id: $0) }
  }

  /// The declarations in this immediate scope.
  var decls: LazyMapCollection<[AnyDeclID], TypedProgram.AnyDecl> {
    whole.scopeToDecls[id, default: []].lazy.map { .init(whole: whole, id: $0) }
  }
}

extension TypedProgram.Node where ID: DeclID {
  /// The scope in which this declaration resides.
  var scope: TypedProgram.AnyScope {
    .init(whole: whole, id: whole.declToScope[id]!)
  }

  /// The type of the declared entity.
  var type: AnyType {
    whole.declTypes[id]!
  }

  /// The implicit captures for the declared entity.
  var implicitCaptures: [ImplicitCapture]? {
    whole.implicitCaptures[id]
  }
}

extension TypedProgram.Node where ID == NodeID<VarDecl> {
  /// The binding decl containing this var.
  var binding: TypedProgram.Node<BindingDecl> {
    .init(whole: whole, id: whole.varToBinding[id]!)
  }
}

extension TypedProgram.Node where ID: ExprID {
  /// The type of this expression
  var type: AnyType {
    whole.exprTypes[id]!
  }
}

extension TypedProgram.Node where ID == NodeID<NameExpr> {
  /// The declaration of this name.
  var decl: DeclRef {
    whole.referredDecls[id]!
  }
}

extension TypedProgram.Node where ID: PatternID {
  /// The names associated with this pattern.
  var names: [(path: [Int], pattern: TypedProgram.Node<NamePattern>)] {
    whole.ast.names(in: id).map({ (path: $0.path, pattern: whole[$0.pattern]) })
  }
}

extension TypedProgram.Node where ID == NodeID<ModuleDecl> {
  /// Collection of (typed) top-level declarations of the module.
  typealias TopLevelDecls = LazyMapSequence<
        FlattenSequence<
            LazyMapSequence<
                [NodeID<TopLevelDeclSet>],
                [AnyDeclID]>>,
        TypedProgram.AnyDecl>

  /// The top-level declarations in the module.
  var topLevelDecls: TopLevelDecls {
    whole.ast.topLevelDecls(id).map({ whole[$0] })
  }
}

extension TypedProgram.Node where ID == NodeID<FunctionDecl> {
  /// The body of the function, containing typed information
  enum Body {

    /// An expression body.
    case expr(TypedProgram.AnyExpr)

    /// A block body.
    case block(TypedProgram.Node<BraceStmt>)

  }

  /// The body of the declaration, if any (in typed formed).
  var body: Body? {
    switch syntax.body {
    case .expr(let expr):
      return .expr(whole[expr])

    case .block(let stmt):
      return .block(whole[stmt])

    case .none:
      return .none
    }
  }
}
