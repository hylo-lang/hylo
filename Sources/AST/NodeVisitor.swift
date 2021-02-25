/// Base protocol for types implementing exhaustive AST visitation.
public protocol NodeVisitor: DeclVisitor, StmtVisitor, ExprVisitor, PatternVisitor, TypeReprVisitor
where Result == DeclResult,
      Result == StmtResult,
      Result == ExprResult,
      Result == PatternResult,
      Result == TypeReprResult
{

  associatedtype Result

}

extension NodeVisitor {

  public func visit(any node: Node) -> Result {
    switch node {
    case let d as Decl    : return d.accept(self)
    case let s as Stmt    : return s.accept(self)
    case let e as Expr    : return e.accept(self)
    case let p as Pattern : return p.accept(self)
    case let t as TypeRepr: return t.accept(self)
    default: fatalError("unreachable")
    }
  }

}

/// Base protocol for types visiting declaration nodes.
public protocol DeclVisitor {

  associatedtype DeclResult

  func visit(_ node: ModuleDecl) -> DeclResult

  func visit(_ node: PatternBindingDecl) -> DeclResult

  func visit(_ node: VarDecl) -> DeclResult

  func visit(_ node: BaseFunDecl) -> DeclResult

  func visit(_ node: FunDecl) -> DeclResult

  func visit(_ node: CtorDecl) -> DeclResult

  func visit(_ node: FunParamDecl) -> DeclResult

  func visit(_ node: GenericTypeDecl) -> DeclResult

  func visit(_ node: NominalTypeDecl) -> DeclResult

  func visit(_ node: ProductTypeDecl) -> DeclResult

  func visit(_ node: ViewTypeDecl) -> DeclResult

  func visit(_ node: AliasTypeDecl) -> DeclResult

  func visit(_ node: GenericParamDecl) -> DeclResult

  func visit(_ node: TypeExtDecl) -> DeclResult

}

/// Base protocol for types visiting statement nodes.
public protocol StmtVisitor {

  associatedtype StmtResult

  func visit(_ node: BraceStmt) -> StmtResult

  func visit(_ node: RetStmt) -> StmtResult

}

/// Base protocol for types visiting expression nodes.
public protocol ExprVisitor {

  associatedtype ExprResult

  func visit(_ node: IntLiteralExpr) -> ExprResult

  func visit(_ node: AssignExpr) -> ExprResult

  func visit(_ node: UnsafeCastExpr) -> ExprResult

  func visit(_ node: TupleExpr) -> ExprResult

  func visit(_ node: CallExpr) -> ExprResult

  func visit(_ node: UnresolvedDeclRefExpr) -> ExprResult

  func visit(_ node: UnresolvedMemberExpr) -> ExprResult

  func visit(_ node: UnresolvedQualDeclRefExpr) -> ExprResult

  func visit(_ node: OverloadedDeclRefExpr) -> ExprResult

  func visit(_ node: DeclRefExpr) -> ExprResult

  func visit(_ node: TypeDeclRefExpr) -> ExprResult

  func visit(_ node: MemberDeclRefExpr) -> ExprResult

  func visit(_ node: TupleMemberExpr) -> ExprResult

  func visit(_ node: AddrOfExpr) -> ExprResult

  func visit(_ node: WildcardExpr) -> ExprResult

  func visit(_ node: ErrorExpr) -> ExprResult

}

/// Base protocol for types visiting pattern nodes.
public protocol PatternVisitor {

  associatedtype PatternResult

  func visit(_ node: NamedPattern) -> PatternResult

  func visit(_ node: TuplePattern) -> PatternResult

  func visit(_ node: WildcardPattern) -> PatternResult

}

/// Base protocol for types visiting type representation nodes.
public protocol TypeReprVisitor {

  associatedtype TypeReprResult

  func visit(_ node: TupleTypeRepr) -> TypeReprResult

  func visit(_ node: FunTypeRepr) -> TypeReprResult

  func visit(_ node: UnionTypeRepr) -> TypeReprResult

  func visit(_ node: ViewCompTypeRepr) -> TypeReprResult

  func visit(_ node: UnqualTypeRepr) -> TypeReprResult

  func visit(_ node: SpecializedTypeRepr) -> TypeReprResult

  func visit(_ node: CompoundTypeRepr) -> TypeReprResult

}
