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

public typealias ValNode = Node
extension AST {
  public typealias Node = ValNode
}

extension TypedProgram {
  @dynamicMemberLookup
  struct Node<ASTPart: AST.Node> {
    let program: TypedProgram
    let id: NodeID<ASTPart>

    var astPart: ASTPart {
      program.ast[id]
    }

    subscript<T>(dynamicMember m: KeyPath<ASTPart,T>) -> T {
      astPart[keyPath: m]
    }

    subscript<T: AST.Node>(
      dynamicMember m: KeyPath<ASTPart, [NodeID<T>]>
    ) -> LazyMapCollection<[NodeID<T>], TypedProgram.Node<T>> {
      astPart[keyPath: m].lazy.map { .init(program: program, id: $0) }
    }

    subscript<T: AST.Node>(
      dynamicMember m: KeyPath<ASTPart, NodeID<T>?>
    ) -> TypedProgram.Node<T>? {
      astPart[keyPath: m].map { .init(program: program, id: $0) }
    }
  }
}

extension TypedProgram.Node where ASTPart: LexicalScope {
  var parent: AnyScopeID? {
    program.scopeToParent[id]
  }

  var decls: [AnyDeclID] {
    program.scopeToDecls[id, default: []]
  }
}

extension TypedProgram.Node where ASTPart: Decl {
  var scope: AnyScopeID {
    program.declToScope[id]!
  }

  var type: Type {
    program.declTypes[id]!
  }
}

extension TypedProgram.Node where ASTPart == VarDecl {
  var binding: TypedProgram.Node<BindingDecl> {
    .init(program: program, id: program.varToBinding[id]!)
  }
}

extension TypedProgram.Node where ASTPart: Expr {
  var type: Type {
    program.exprTypes[id]!
  }
}

extension TypedProgram.Node where ASTPart == NameExpr {
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
