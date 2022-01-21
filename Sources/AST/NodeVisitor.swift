/// Base protocol for types implementing exhaustive AST visitation.
public protocol NodeVisitor: DeclVisitor, StmtVisitor, ExprVisitor, PatternVisitor, SignVisitor
where Result == DeclResult,
      Result == StmtResult,
      Result == ExprResult,
      Result == PatternResult,
      Result == SignResult
{

  associatedtype Result

}

extension NodeVisitor {

  mutating public func visit(any node: Node) -> Result {
    switch node {
    case let d as Decl    : return d.accept(&self)
    case let s as Stmt    : return s.accept(&self)
    case let e as Expr    : return e.accept(&self)
    case let p as Pattern : return p.accept(&self)
    case let t as Sign    : return t.accept(&self)
    default: fatalError("unreachable")
    }
  }

}

/// Base protocol for types visiting declaration nodes.
public protocol DeclVisitor {

  associatedtype DeclResult

  mutating func visit(_ node: ModuleDecl) -> DeclResult

  mutating func visit(_ node: ImportDecl) -> DeclResult

  mutating func visit(_ node: PatternBindingDecl) -> DeclResult

  mutating func visit(_ node: VarDecl) -> DeclResult

  mutating func visit(_ node: BaseFunDecl) -> DeclResult

  mutating func visit(_ node: FunDecl) -> DeclResult

  mutating func visit(_ node: CtorDecl) -> DeclResult

  mutating func visit(_ node: CaptureDecl) -> DeclResult

  mutating func visit(_ node: FunParamDecl) -> DeclResult

  mutating func visit(_ node: GenericTypeDecl) -> DeclResult

  mutating func visit(_ node: NominalTypeDecl) -> DeclResult

  mutating func visit(_ node: ProductTypeDecl) -> DeclResult

  mutating func visit(_ node: ViewTypeDecl) -> DeclResult

  mutating func visit(_ node: AliasTypeDecl) -> DeclResult

  mutating func visit(_ node: AbstractTypeDecl) -> DeclResult

  mutating func visit(_ node: GenericParamDecl) -> DeclResult

  mutating func visit(_ node: TypeExtnDecl) -> DeclResult

}

/// Base protocol for types visiting statement nodes.
public protocol StmtVisitor {

  associatedtype StmtResult

  mutating func visit(_ node: BraceStmt) -> StmtResult

  mutating func visit(_ node: RetStmt) -> StmtResult

  mutating func visit(_ node: IfStmt) -> StmtResult

  mutating func visit(_ node: MatchCaseStmt) -> StmtResult

}

/// Base protocol for types visiting expression nodes.
public protocol ExprVisitor {

  associatedtype ExprResult

  mutating func visit(_ node: BoolLiteralExpr) -> ExprResult

  mutating func visit(_ node: IntLiteralExpr) -> ExprResult

  mutating func visit(_ node: FloatLiteralExpr) -> ExprResult

  mutating func visit(_ node: StringLiteralExpr) -> ExprResult

  mutating func visit(_ node: AssignExpr) -> ExprResult

  mutating func visit(_ node: BaseCastExpr) -> ExprResult

  mutating func visit(_ node: RuntimeCastExpr) -> ExprResult

  mutating func visit(_ node: TupleExpr) -> ExprResult

  mutating func visit(_ node: CallExpr) -> ExprResult

  mutating func visit(_ node: UnresolvedDeclRefExpr) -> ExprResult

  mutating func visit(_ node: UnresolvedMemberExpr) -> ExprResult

  mutating func visit(_ node: UnresolvedQualDeclRefExpr) -> ExprResult

  mutating func visit(_ node: OverloadedDeclRefExpr) -> ExprResult

  mutating func visit(_ node: DeclRefExpr) -> ExprResult

  mutating func visit(_ node: TypeDeclRefExpr) -> ExprResult

  mutating func visit(_ node: MemberDeclRefExpr) -> ExprResult

  mutating func visit(_ node: TupleMemberExpr) -> ExprResult

  mutating func visit(_ node: AsyncExpr) -> ExprResult

  mutating func visit(_ node: AwaitExpr) -> ExprResult

  mutating func visit(_ node: AddrOfExpr) -> ExprResult

  mutating func visit(_ node: MatchExpr) -> ExprResult

  mutating func visit(_ node: WildcardExpr) -> ExprResult

  mutating func visit(_ node: ErrorExpr) -> ExprResult

}

/// Base protocol for types visiting pattern nodes.
public protocol PatternVisitor {

  associatedtype PatternResult

  mutating func visit(_ node: NamedPattern) -> PatternResult

  mutating func visit(_ node: TuplePattern) -> PatternResult

  mutating func visit(_ node: BindingPattern) -> PatternResult

  mutating func visit(_ node: WildcardPattern) -> PatternResult

}

/// Base protocol for types visiting type signature nodes.
public protocol SignVisitor {

  associatedtype SignResult

  mutating func visit(_ node: TupleSign) -> SignResult

  mutating func visit(_ node: FunSign) -> SignResult

  mutating func visit(_ node: FunParamSign) -> SignResult

  mutating func visit(_ node: AsyncSign) -> SignResult

  mutating func visit(_ node: UnionSign) -> SignResult

  mutating func visit(_ node: ViewCompSign) -> SignResult

  mutating func visit(_ node: BareIdentSign) -> SignResult

  mutating func visit(_ node: SpecializedIdentSign) -> SignResult

  mutating func visit(_ node: CompoundIdentSign) -> SignResult

  mutating func visit(_ node: ErrorSign) -> SignResult

}
