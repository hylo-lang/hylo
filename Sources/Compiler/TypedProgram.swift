/// A data structure representing a typed Val program ready to be lowered.
public struct TypedProgram: Program {

  public let ast: AST

  public let scopeToParent: ASTProperty<AnyScopeID>

  public let scopeToDecls: ASTProperty<[AnyDeclID]>

  public let declToScope: DeclProperty<AnyScopeID>

  public let varToBinding: [NodeID<VarDecl>: NodeID<BindingDecl>]

  /// The overarching type of each declaration.
  public let declTypes: DeclProperty<Type>

  /// The type of each expression.
  public let exprTypes: ExprProperty<Type>

  /// A map from name expression to its referred declaration.
  public let referredDecls: [NodeID<NameExpr>: DeclRef]

  /// Creates a typed program from a scoped program and property maps describing type annotations.
  public init(
    annotating program: ScopedProgram,
    declTypes: DeclProperty<Type>,
    exprTypes: ExprProperty <Type>,
    referredDecls: [NodeID<NameExpr>: DeclRef]
  ) {
    self.ast = program.ast
    self.scopeToParent = program.scopeToParent
    self.scopeToDecls = program.scopeToDecls
    self.declToScope = program.declToScope
    self.varToBinding = program.varToBinding
    self.declTypes = declTypes
    self.exprTypes = exprTypes
    self.referredDecls = referredDecls
  }
}

// Renaming dance

public typealias ValNode = Node
extension AST {
  public typealias Node = ValNode
}

extension TypedProgram {
  /// A node whose corresponding `AST.Node` type is statically known.
  @dynamicMemberLookup
  struct SomeNode<ID: NodeIDProtocol> {
    /// The program in which this node resides.
    let program: TypedProgram

    /// The node's identity in the AST.
    let id: ID
  }

  typealias AnyScope = SomeNode<AnyScopeID>
  typealias AnyDecl = SomeNode<AnyDeclID>
  typealias AnyTypeExpr = SomeNode<AnyTypeExprID>

  typealias Node<T: AST.Node> = SomeNode<NodeID<T>>
}

extension TypedProgram.SomeNode {
  /// The value of the node in the AST.
  func astPart<T>() -> T where ID == NodeID<T> {
    program.ast[id]
  }

  /// Accesses the given member of the corresponding AST node.
  subscript<T, U>(dynamicMember m: KeyPath<T, U>) -> U where ID == NodeID<T> {
    astPart()[keyPath: m]
  }

  /// Accesses the given member of the corresponding AST node as a corresponding lazy collection
  /// of `TypedProgram.Node`s.
  subscript<T, U: AST.Node>(
    dynamicMember m: KeyPath<T, [NodeID<U>]>
  ) -> LazyMapCollection<[NodeID<U>], TypedProgram.Node<U>>
    where ID == NodeID<T>
  {
    astPart()[keyPath: m].lazy.map { .init(program: program, id: $0) }
  }

  /// Accesses the given member of the corresponding AST node as a corresponding
  /// `TypedProgram.Node?`.
  subscript<T, U: AST.Node>(
    dynamicMember m: KeyPath<T, NodeID<U>?>
  ) -> TypedProgram.Node<U>?
    where ID == NodeID<T>
  {
    astPart()[keyPath: m].map { .init(program: program, id: $0) }
  }

  /// Accesses the given member of the corresponding AST node as a corresponding
  /// `AnyTypeExpr?`.
  subscript<T>(
    dynamicMember m: KeyPath<T, AnyTypeExprID?>
  ) -> TypedProgram.AnyTypeExpr?
    where ID == NodeID<T>
  {
    astPart()[keyPath: m].map { .init(program: program, id: $0) }
  }
}

extension TypedProgram.SomeNode where ID: ScopeID {
  var parentID: AnyScopeID? {
    program.scopeToParent[id]
  }

  var parent: TypedProgram.AnyScope? {
    parentID.map { .init(program: program, id: $0) }
  }

  var declIDs: [AnyDeclID] {
    program.scopeToDecls[id, default: []]
  }

  var decls: LazyMapCollection<[AnyDeclID], TypedProgram.AnyDecl> {
    declIDs.lazy.map { .init(program: program, id: $0) }
  }
}

extension TypedProgram.Node where ID: DeclID {
  var scope: TypedProgram.AnyScope {
    .init(program: program, id: program.declToScope[id]!)
  }

  var type: Type {
    program.declTypes[id]!
  }
}

extension TypedProgram.Node where ID == NodeID<VarDecl> {
  var binding: TypedProgram.Node<BindingDecl> {
    .init(program: program, id: program.varToBinding[id]!)
  }
}

extension TypedProgram.Node where ID: ExprID {
  var type: Type {
    program.exprTypes[id]!
  }
}

extension TypedProgram.Node where ID == NodeID<NameExpr> {
  /// The declaration of this name.
  var decl: DeclRef {
    program.referredDecls[id]!
  }
}

func crazyTest0(x: TypedProgram.Node<FunDecl>) -> [TypedProgram.Node<ParameterDecl>] {
  print(x.type)
  let p = x.parameters
  return Array(p)
}

func crazyTest1(x: TypedProgram.Node<FunDecl>) -> TypedProgram.Node<ParameterDecl>? {
  let p = x.implicitReceiverDecl
  return p
}

func crazyTest2(x: TypedProgram.AnyDecl) -> TypedProgram.AnyScope? {
  return x.scope.parent
}
