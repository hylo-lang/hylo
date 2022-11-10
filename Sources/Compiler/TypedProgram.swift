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

/// AST nodes packaged with all the node-specific information provided by `TypedProgram`.
protocol TypedProgramNode {
  /// A type that can be used to identify a corresponding part of the AST.
  associatedtype ID: NodeIDProtocol

  /// The whole program of which this node is a part.
  var program: TypedProgram { get }

  /// The id of this node in the AST.
  var id: ID { get }
}

extension TypedProgram {
  /// A node whose corresponding `AST.Node` type is statically known.
  @dynamicMemberLookup
  struct Node<ASTPart: AST.Node>: TypedProgramNode {
    /// The program in which this node resides.
    let program: TypedProgram

    /// The node's identity in the AST.
    let id: NodeID<ASTPart>

    /// The value of the node in the AST.
    var astPart: ASTPart {
      program.ast[id]
    }

    /// Accesses the given member of the corresponding AST node.
    subscript<T>(dynamicMember m: KeyPath<ASTPart,T>) -> T {
      astPart[keyPath: m]
    }

    /// Accesses the given member of the corresponding AST node as a corresponding lazy collection
    /// of `TypedProgram.Node`s.
    subscript<T: AST.Node>(
      dynamicMember m: KeyPath<ASTPart, [NodeID<T>]>
    ) -> LazyMapCollection<[NodeID<T>], TypedProgram.Node<T>> {
      astPart[keyPath: m].lazy.map { .init(program: program, id: $0) }
    }

    /// Accesses the given member of the corresponding AST node as a corresponding
    /// `TypedProgram.Node?`.
    subscript<T: AST.Node>(
      dynamicMember m: KeyPath<ASTPart, NodeID<T>?>
    ) -> TypedProgram.Node<T>? {
      astPart[keyPath: m].map { .init(program: program, id: $0) }
    }

    /// Accesses the given member of the corresponding AST node as a corresponding
    /// `AnyTypeExpr?`.
    subscript(
      dynamicMember m: KeyPath<ASTPart, AnyTypeExprID?>
    ) -> AnyTypeExpr? {
      astPart[keyPath: m].map { .init(program: program, id: $0) }
    }
  }

  struct AnyScope: TypedProgramNode {
    let program: TypedProgram
    let id: AnyScopeID
  }

  struct AnyDecl: TypedProgramNode {
    let program: TypedProgram
    let id: AnyDeclID
  }

  struct AnyTypeExpr: TypedProgramNode {
    let program: TypedProgram
    let id: AnyTypeExprID
  }
}

extension TypedProgramNode where ID: ScopeID {
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

extension TypedProgramNode where ID: DeclID {
  var scope: TypedProgram.AnyScope {
    .init(program: program, id: program.declToScope[id]!)
  }

  var type: Type {
    program.declTypes[id]!
  }
}

extension TypedProgramNode where ID == NodeID<VarDecl> {
  var binding: TypedProgram.Node<BindingDecl> {
    .init(program: program, id: program.varToBinding[id]!)
  }
}

extension TypedProgramNode where ID: ExprID {
  var type: Type {
    program.exprTypes[id]!
  }
}

extension TypedProgramNode where ID == NodeID<NameExpr> {
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
