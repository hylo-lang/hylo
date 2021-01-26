/// Base protocol for types implementing exhaustive AST visitation.
public protocol NodeVisitor {

  associatedtype Result

  // MARK: Declarations

  func visit(_ node: Module) -> Result

  func visit(_ node: PatternBindingDecl) -> Result

  func visit(_ node: VarDecl) -> Result

  func visit(_ node: AbstractFunDecl) -> Result

  func visit(_ node: FunDecl) -> Result

  func visit(_ node: CtorDecl) -> Result

  func visit(_ node: FunParamDecl) -> Result

  func visit(_ node: AbstractNominalTypeDecl) -> Result

  func visit(_ node: ProductTypeDecl) -> Result

  func visit(_ node: ViewTypeDecl) -> Result

  func visit(_ node: GenericParamDecl) -> Result

  func visit(_ node: TypeExtDecl) -> Result

  // MARK: Statements

  func visit(_ node: BraceStmt) -> Result

  func visit(_ node: RetStmt) -> Result

  // MARK: Expressions

  func visit(_ node: IntLiteralExpr) -> Result

  func visit(_ node: AssignExpr) -> Result

  func visit(_ node: CallExpr) -> Result

  func visit(_ node: UnresolvedDeclRefExpr) -> Result

  func visit(_ node: UnresolvedMemberExpr) -> Result

  func visit(_ node: QualDeclRefExpr) -> Result

  func visit(_ node: OverloadedDeclRefExpr) -> Result

  func visit(_ node: DeclRefExpr) -> Result

  func visit(_ node: TypeDeclRefExpr) -> Result

  func visit(_ node: MemberRefExpr) -> Result

  func visit(_ node: AddrOfExpr) -> Result

  func visit(_ node: WildcardExpr) -> Result

  // MARK: Patterns

  func visit(_ node: NamedPattern) -> Result

  func visit(_ node: TuplePattern) -> Result

  func visit(_ node: WildcardPattern) -> Result

  // MARK: Type representations

  func visit(_ node: BuiltinTypeRepr) -> Result

  func visit(_ node: UnqualTypeRepr) -> Result

  func visit(_ node: CompoundTypeRepr) -> Result

}
