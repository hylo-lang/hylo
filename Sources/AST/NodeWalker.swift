/// A protocol to implement "event-based" AST visitors and transformers.
public protocol NodeWalker: NodeVisitor where Result == Bool {

  /// The parent of the node being visited.
  var parent: Node? { get set }

  /// The innermost declaration space in which the next node will be visited.
  var innermostSpace: DeclSpace? { get set }

  // MARK: Event handlers

  /// This method is called when the walker is about to visit a declaration.
  ///
  /// - Parameter decl: The declaration that will be visited.
  /// - Returns: A Boolean value that indicates whether the walker should visit `decl`.
  ///   The default implementation returns `true`.
  mutating func willVisit(_ decl: Decl) -> Bool

  /// This method is called after the walker visited a declaration.
  ///
  /// - Parameter decl: The declaration that was visited.
  /// - Returns: A Boolean value that indicates whether the walker should proceed to the next node.
  ///   The default implementation returns `true`.
  mutating func didVisit(_ decl: Decl) -> Bool

  /// This method is called when the walker is about to visit a statement.
  ///
  /// - Parameter stmt: The statement that will be visited.
  /// - Returns: A Boolean value that indicates whether the walker should visit `stmt`.
  ///   The default implementation returns `true`.
  mutating func willVisit(_ stmt: Stmt) -> Bool

  /// This method is called after the walker visited a statement.
  ///
  /// - Parameter stmt: The statement that was visited.
  /// - Returns: A Boolean value that indicates whether the walker should proceed to the next node.
  ///   The default implementation returns `true`.
  mutating func didVisit(_ stmt: Stmt) -> Bool

  /// This method is called when the walker is about to visit an expression.
  ///
  /// - Parameter expr: The expression that will be visited.
  /// - Returns: `(flag, node)`, where `node` subsitutes `expr` in the AST, and `flag` is a
  ///   Boolean value that indicates whether the walker should visit `node`. The default
  ///   implementation returns `(true, expr)`.
  mutating func willVisit(_ expr: Expr) -> (shouldWalk: Bool, nodeBefore: Expr)

  /// This method is called after the walker visited an expression.
  ///
  /// - Parameter expr: The expression that was visited.
  /// - Returns: `(flag, node)` where `node` subsitutes `expr` in the AST, and `flag` is a
  ///   Boolean value that indicates whether the walker should proceed to the next node. The
  ///   default implementation returns `(true, expr)`.
  mutating func didVisit(_ expr: Expr) -> (shouldContinue: Bool, nodeAfter: Expr)

  /// This method is called when the walker is about to visit a pattern.
  ///
  /// - Parameter pattern: The patterm that will be visited.
  /// - Returns: `(flag, node)` where `node` subsitutes `pattern` in the AST, and `flag` is a
  ///   Boolean value that indicates whether the walker should visit `node`. The default
  ///   implementation returns `(true, pattern)`.
  mutating func willVisit(_ pattern: Pattern) -> (shouldWalk: Bool, nodeBefore: Pattern)

  /// This method is called after the walker visited a pattern.
  ///
  /// - Parameter pattern: The patterm that was visited.
  /// - Returns: `(flag, node)` where `node` subsitutes `pattern` in the AST, and `flag` is a
  ///   Boolean value that indicates whether the walker should proceed to the next node. The
  ///   default implementation returns `(true, pattern)`.
  mutating func didVisit(_ pattern: Pattern) -> (shouldContinue: Bool, nodeAfter: Pattern)

  /// This method is called when the walker is about to visit a type signature.
  ///
  /// - Parameter sign: The type signature that will be visited.
  /// - Returns: `(flag, node)`, where `node` subsitutes `sign` in the AST, and `flag` is a
  ///   Boolean value that indicates whether the walker should visit `node`. The default
  ///   implementation returns `(true, expr)`.
  mutating func willVisit(_ sign: Sign) -> (shouldWalk: Bool, nodeBefore: Sign)

  /// This method is called after the walker visited a type signature.
  ///
  /// - Parameter expr: The type signature that was visited.
  /// - Returns: `(flag, node)` where `node` subsitutes `sign` in the AST, and `flag` is a
  ///   Boolean value that indicates whether the walker should proceed to the next node. The
  ///   default implementation returns `(true, sign)`.
  mutating func didVisit(_ sign: Sign) -> (shouldContinue: Bool, nodeAfter: Sign)

  /// This method is called when the walker is about to visit a generic clause.
  ///
  /// - Parameter clause: The generic clause that will be visited.
  /// - Returns: A Boolean value that indicates whether the walker should visit `clause`.
  ///   The default implementation returns `true`.
  mutating func willVisit(_ clause: GenericClause) -> Bool

  /// This method is called after the walker visited a generic clause.
  ///
  /// - Parameter clause: The generic clause that was visited.
  /// - Returns: A Boolean value that indicates whether the walker should proceed to the next node.
  ///   The default implementation returns `true`.
  mutating func didVisit(_ clause: GenericClause) -> Bool

}

extension NodeWalker {

  @discardableResult
  public mutating func walk(decl: Decl) -> Bool {
    // Fire the `willVisit` event.
    guard willVisit(decl) else { return true }

    // Visit the node's children.
    guard decl.accept(&self) else { return false }

    // Fire the `didVisit` event.
    return didVisit(decl)
  }

  @discardableResult
  public mutating func walk(stmt: Stmt) -> Bool {
    // Fire the `willVisit` event.
    guard willVisit(stmt) else { return true }

    // Visit the node's children.
    guard stmt.accept(&self) else { return false }

    // Fire the `didVisit` event.
    return didVisit(stmt)
  }

  @discardableResult
  public mutating func walk(expr: Expr) -> (Bool, Expr) {
    // Fire the `willVisit` event.
    let (shouldVisit, substitute) = willVisit(expr)
    guard shouldVisit else {
      return (true, substitute)
    }

    // Visit the node's children.
    guard substitute.accept(&self) else {
      return (false, substitute)
    }

    // Fire the `didVisit` event.
    return didVisit(substitute)
  }

  @discardableResult
  public mutating func walk(pattern: Pattern) -> (Bool, Pattern) {
    // Fire the `willVisit` event.
    let (shouldVisit, substitute) = willVisit(pattern)
    guard shouldVisit else {
      return (true, substitute)
    }

    // Visit the node's children.
    guard substitute.accept(&self) else {
      return (false, substitute)
    }

    // Fire the `didVisit` event.
    return didVisit(substitute)
  }

  @discardableResult
  public mutating func walk(sign: Sign) -> (Bool, Sign) {
    // Fire the `willVisit` event.
    let (shouldVisit, substitute) = willVisit(sign)
    guard shouldVisit else {
      return (true, substitute)
    }

    // Visit the node's children.
    guard substitute.accept(&self) else {
      return (false, substitute)
    }

    // Fire the `didVisit` event.
    return didVisit(substitute)
  }

  public mutating func willVisit(_ decl: Decl) -> Bool {
    return true
  }

  public mutating func didVisit(_ decl: Decl) -> Bool {
    return true
  }

  public mutating func willVisit(_ stmt: Stmt) -> Bool {
    return true
  }

  public mutating func didVisit(_ stmt: Stmt) -> Bool {
    return true
  }

  public mutating func willVisit(_ expr: Expr) -> (shouldWalk: Bool, nodeBefore: Expr) {
    return (true, expr)
  }

  public mutating func didVisit(_ expr: Expr) -> (shouldContinue: Bool, nodeAfter: Expr) {
    return (true, expr)
  }

  public mutating func willVisit(_ pattern: Pattern) -> (shouldWalk: Bool, nodeBefore: Pattern) {
    return (true, pattern)
  }

  public mutating func didVisit(_ pattern: Pattern) -> (shouldContinue: Bool, nodeAfter: Pattern) {
    return (true, pattern)
  }

  public mutating func willVisit(_ sign: Sign) -> (shouldWalk: Bool, nodeBefore: Sign) {
    return (true, sign)
  }

  public mutating func didVisit(_ sign: Sign) -> (shouldContinue: Bool, nodeAfter: Sign) {
    return (true, sign)
  }

  public mutating func willVisit(_ clause: GenericClause) -> Bool {
    return true
  }

  public mutating func didVisit(_ clause: GenericClause) -> Bool {
    return true
  }

  public mutating func visit(_ node: ModuleDecl) -> Bool {
    return traverse(node)
  }

  public mutating func traverse(_ node: ModuleDecl) -> Bool {
    let prevParent = parent
    parent = node
    innermostSpace = node
    defer {
      parent = prevParent
      innermostSpace = nil
    }

    for i in node.indices {
      guard walk(decl: node[i]) else { return false }
    }

    return true
  }

  public mutating func visit(_ node: ImportDecl) -> Bool {
    return traverse(node)
  }

  public mutating func traverse(_ node: ImportDecl) -> Bool {
    let prevParent = parent
    parent = node
    defer {
      parent = prevParent
    }

    return true
  }

  public mutating func visit(_ node: PatternBindingDecl) -> Bool {
    return traverse(node)
  }

  public mutating func traverse(_ node: PatternBindingDecl) -> Bool {
    let prevParent = parent
    parent = node
    defer { parent = prevParent }

    var shouldContinue: Bool
    (shouldContinue, node.pattern) = walk(pattern: node.pattern)
    guard shouldContinue else { return false }

    if let signature = node.sign {
      (shouldContinue, node.sign) = walk(sign: signature)
      guard shouldContinue else { return false }
    }

    if let initializer = node.initializer {
      (shouldContinue, node.initializer) = walk(expr: initializer)
      guard shouldContinue else { return false }
    }

    return true
  }

  public mutating func visit(_ node: VarDecl) -> Bool {
    return true
  }

  public mutating func visit(_ node: BaseFunDecl) -> Bool {
    return traverse(node)
  }

  public mutating func traverse(_ node: BaseFunDecl) -> Bool {
    let prevParent = parent
    parent = node
    innermostSpace = node
    defer {
      parent = prevParent
      innermostSpace = innermostSpace?.parentDeclSpace
    }

    if let clause = node.genericClause, willVisit(clause) {
      guard visit(clause) else { return false }
      guard didVisit(clause) else { return false }
    }

    for i in 0 ..< node.params.count {
      guard walk(decl: node.params[i]) else { return false }
    }

    var shouldContinue: Bool
    if let signature = node.retSign {
      (shouldContinue, node.retSign) = walk(sign: signature)
      guard shouldContinue else { return false }
    }

    if let body = node.body {
      guard walk(stmt: body) else { return false }
    }

    return true
  }

  public mutating func visit(_ node: FunDecl) -> Bool {
    return traverse(node)
  }

  public mutating func visit(_ node: CtorDecl) -> Bool {
    return traverse(node)
  }

  public mutating func visit(_ node: CaptureDecl) -> Bool {
    return true
  }

  public mutating func visit(_ node: FunParamDecl) -> Bool {
    return traverse(node)
  }

  public mutating func traverse(_ node: FunParamDecl) -> Bool {
    let prevParent = parent
    parent = node
    defer { parent = prevParent }

    var shouldContinue: Bool
    if let signature = node.sign {
      (shouldContinue, node.sign) = walk(sign: signature)
      guard shouldContinue else { return false }
    }

    return true
  }

  public mutating func visit(_ node: GenericTypeDecl) -> Bool {
    switch node {
    case let decl as NominalTypeDecl  : return visit(decl)
    case let decl as AliasTypeDecl    : return visit(decl)
    default: fatalError("unreachable")
    }
  }

  public mutating func visit(_ node: NominalTypeDecl) -> Bool {
    switch node {
    case let decl as ProductTypeDecl  : return visit(decl)
    case let decl as ViewTypeDecl     : return visit(decl)
    default: fatalError("unreachable")
    }
  }

  public mutating func visit(_ node: ProductTypeDecl) -> Bool {
    return traverse(node)
  }

  public mutating func traverse(_ node: ProductTypeDecl) -> Bool {
    let prevParent = parent
    parent = node
    innermostSpace = node
    defer {
      parent = prevParent
      innermostSpace = innermostSpace?.parentDeclSpace
    }

    if let clause = node.genericClause, willVisit(clause) {
      guard visit(clause) else { return false }
      guard didVisit(clause) else { return false }
    }

    var shouldContinue: Bool
    for i in 0 ..< node.inheritances.count {
      (shouldContinue, node.inheritances[i]) = walk(sign: node.inheritances[i])
      guard shouldContinue else { return false }
    }

    for i in 0 ..< node.members.count {
      guard walk(decl: node.members[i]) else { return false }
    }

    return true
  }

  public mutating func visit(_ node: ViewTypeDecl) -> Bool {
    return traverse(node)
  }

  public mutating func traverse(_ node: ViewTypeDecl) -> Bool {
    let prevParent = parent
    parent = node
    innermostSpace = node
    defer {
      parent = prevParent
      innermostSpace = innermostSpace?.parentDeclSpace
    }

    var shouldContinue: Bool
    for i in 0 ..< node.inheritances.count {
      (shouldContinue, node.inheritances[i]) = walk(sign: node.inheritances[i])
      guard shouldContinue else { return false }
    }

    for i in 0 ..< node.members.count {
      guard walk(decl: node.members[i]) else { return false }
    }

    return true
  }

  public mutating func visit(_ node: AliasTypeDecl) -> Bool {
    return traverse(node)
  }

  public mutating func traverse(_ node: AliasTypeDecl) -> Bool {
    let prevParent = parent
    parent = node
    innermostSpace = node
    defer {
      parent = prevParent
      innermostSpace = innermostSpace?.parentDeclSpace
    }

    if let clause = node.genericClause, willVisit(clause) {
      guard visit(clause) else { return false }
      guard didVisit(clause) else { return false }
    }

    var shouldContinue: Bool
    for i in 0 ..< node.inheritances.count {
      (shouldContinue, node.inheritances[i]) = walk(sign: node.inheritances[i])
      guard shouldContinue else { return false }
    }

    (shouldContinue, node.aliasedSign) = walk(sign: node.aliasedSign)
    guard shouldContinue else { return false }

    return true
  }

  public mutating func visit(_ node: AbstractTypeDecl) -> Bool {
    return traverse(node)
  }

  public mutating func traverse(_ node: AbstractTypeDecl) -> Bool {
    let prevParent = parent
    parent = node
    defer {
      parent = prevParent
    }

    var shouldContinue: Bool
    for i in 0 ..< node.inheritances.count {
      (shouldContinue, node.inheritances[i]) = walk(sign: node.inheritances[i])
      guard shouldContinue else { return false }
    }

    for i in 0 ..< node.typeReqs.count {
      (shouldContinue, node.typeReqs[i].lhs) = walk(sign: node.typeReqs[i].lhs)
        as! (Bool, IdentSign)
      guard shouldContinue else { return false }

      (shouldContinue, node.typeReqs[i].rhs) = walk(sign: node.typeReqs[i].rhs)
      guard shouldContinue else { return false }
    }

    return true
  }

  public mutating func visit(_ node: GenericParamDecl) -> Bool {
    return true
  }

  public mutating func visit(_ node: TypeExtnDecl) -> Bool {
    return traverse(node)
  }

  public mutating func traverse(_ node: TypeExtnDecl) -> Bool {
    let prevParent = parent
    parent = node
    innermostSpace = node
    defer {
      parent = prevParent
      innermostSpace = innermostSpace?.parentDeclSpace
    }

    var shouldContinue: Bool
    (shouldContinue, node.extendedIdent) = walk(sign: node.extendedIdent) as! (Bool, IdentSign)
    guard shouldContinue else { return false }

    for i in 0 ..< node.members.count {
      guard walk(decl: node.members[i]) else { return false }
    }

    return true
  }

  public mutating func visit(_ node: BraceStmt) -> Bool {
    return traverse(node)
  }

  public mutating func traverse(_ node: BraceStmt) -> Bool {
    let prevParent = parent
    parent = node
    innermostSpace = node
    defer {
      parent = prevParent
      innermostSpace = innermostSpace?.parentDeclSpace
    }

    var shouldContinue: Bool
    for i in 0 ..< node.stmts.count {
      switch node.stmts[i] {
      case let decl as Decl:
        shouldContinue = walk(decl: decl)
      case let stmt as Stmt:
        shouldContinue = walk(stmt: stmt)
      case let expr as Expr:
        (shouldContinue, node.stmts[i]) = walk(expr: expr)
      default:
        fatalError("unreachable")
      }
      guard shouldContinue else { return false }
    }

    return true
  }

  public mutating func visit(_ node: RetStmt) -> Bool {
    return traverse(node)
  }

  public mutating func traverse(_ node: RetStmt) -> Bool {
    let prevParent = parent
    parent = node
    defer { parent = prevParent }

    var shouldContinue: Bool
    if let value = node.value {
      (shouldContinue, node.value) = walk(expr: value)
      guard shouldContinue else { return false }
    }

    return true
  }

  public mutating func visit(_ node: MatchCaseStmt) -> Bool {
    return traverse(node)
  }

  public mutating func traverse(_ node: MatchCaseStmt) -> Bool {
    let prevParent = parent
    parent = node
    defer { parent = prevParent }

    var shouldContinue: Bool
    (shouldContinue, node.pattern) = walk(pattern: node.pattern)
    guard shouldContinue else { return false }

    if let condition = node.condition {
      (shouldContinue, node.condition) = walk(expr: condition)
      guard shouldContinue else { return false }
    }

    guard walk(stmt: node.body) else { return false }

    return true
  }

  public mutating func visit(_ node: BoolLiteralExpr) -> Bool {
    return true
  }

  public mutating func visit(_ node: IntLiteralExpr) -> Bool {
    return true
  }

  public mutating func visit(_ node: FloatLiteralExpr) -> Bool {
    return true
  }

  public mutating func visit(_ node: StringLiteralExpr) -> Bool {
    return true
  }

  public mutating func visit(_ node: AssignExpr) -> Bool {
    return traverse(node)
  }

  public mutating func traverse(_ node: AssignExpr) -> Bool {
    let prevParent = parent
    parent = node
    defer { parent = prevParent }

    var shouldContinue: Bool
    (shouldContinue, node.lvalue) = walk(expr: node.lvalue)
    guard shouldContinue else { return false }

    (shouldContinue, node.rvalue) = walk(expr: node.rvalue)
    guard shouldContinue else { return false }

    return true
  }

  public mutating func visit(_ node: BaseCastExpr) -> Bool {
    return traverse(node)
  }

  public mutating func traverse(_ node: BaseCastExpr) -> Bool {
    let prevParent = parent
    parent = node
    defer { parent = prevParent }

    var shouldContinue: Bool
    (shouldContinue, node.value) = walk(expr: node.value)
    guard shouldContinue else { return false }

    (shouldContinue, node.sign) = walk(sign: node.sign)
    guard shouldContinue else { return false }

    return true
  }

  public mutating func visit(_ node: DynCastExpr) -> Bool {
    return visit(node as BaseCastExpr)
  }

  public mutating func visit(_ node: UnsafeCastExpr) -> Bool {
    return visit(node as BaseCastExpr)
  }

  public mutating func visit(_ node: TupleExpr) -> Bool {
    return traverse(node)
  }

  public mutating func traverse(_ node: TupleExpr) -> Bool {
    let prevParent = parent
    parent = node
    defer { parent = prevParent }

    var shouldContinue: Bool
    for i in 0 ..< node.elems.count {
      (shouldContinue, node.elems[i].value) = walk(expr: node.elems[i].value)
      guard shouldContinue else { return false }
    }

    return true
  }

  public mutating func visit(_ node: CallExpr) -> Bool {
    return traverse(node)
  }

  public mutating func traverse(_ node: CallExpr) -> Bool {
    let prevParent = parent
    parent = node
    defer { parent = prevParent }

    var shouldContinue: Bool
    (shouldContinue, node.fun) = walk(expr: node.fun)
    guard shouldContinue else { return false }

    for i in 0 ..< node.args.count {
      (shouldContinue, node.args[i].value) = walk(expr: node.args[i].value)
      guard shouldContinue else { return false }
    }

    return true
  }

  public mutating func visit(_ node: UnresolvedDeclRefExpr) -> Bool {
    return true
  }

  public mutating func visit(_ node: UnresolvedQualDeclRefExpr) -> Bool {
    return traverse(node)
  }

  public mutating func traverse(_ node: UnresolvedQualDeclRefExpr) -> Bool {
    let prevParent = parent
    parent = node
    defer { parent = prevParent }

    var shouldContinue: Bool
    (shouldContinue, node.namespace) = walk(sign: node.namespace) as! (Bool, IdentSign)
    return shouldContinue
  }

  public mutating func visit(_ node: OverloadedDeclRefExpr) -> Bool {
    return true
  }

  public mutating func visit(_ node: DeclRefExpr) -> Bool {
    return true
  }

  public mutating func visit(_ node: TypeDeclRefExpr) -> Bool {
    return true
  }

  public mutating func visit(_ node: UnresolvedMemberExpr) -> Bool {
    return traverse(node)
  }

  public mutating func traverse(_ node: UnresolvedMemberExpr) -> Bool {
    let prevParent = parent
    parent = node
    defer { parent = prevParent }

    var shouldContinue: Bool
    (shouldContinue, node.base) = walk(expr: node.base)
    return shouldContinue
  }

  public mutating func visit(_ node: MemberDeclRefExpr) -> Bool {
    return traverse(node)
  }

  public mutating func traverse(_ node: MemberDeclRefExpr) -> Bool {
    let prevParent = parent
    parent = node
    defer { parent = prevParent }

    var shouldContinue: Bool
    (shouldContinue, node.base) = walk(expr: node.base)
    return shouldContinue
  }

  public mutating func visit(_ node: TupleMemberExpr) -> Bool {
    return traverse(node)
  }

  public mutating func traverse(_ node: TupleMemberExpr) -> Bool {
    let prevParent = parent
    parent = node
    defer { parent = prevParent }

    var shouldContinue: Bool
    (shouldContinue, node.base) = walk(expr: node.base)
    return shouldContinue
  }

  public mutating func visit(_ node: AsyncExpr) -> Bool {
    return traverse(node)
  }

  public mutating func traverse(_ node: AsyncExpr) -> Bool {
    let prevParent = parent
    parent = node
    defer { parent = prevParent }

    return walk(decl: node.body)
  }

  public mutating func visit(_ node: AwaitExpr) -> Bool {
    return traverse(node)
  }

  public mutating func traverse(_ node: AwaitExpr) -> Bool {
    let prevParent = parent
    parent = node
    defer { parent = prevParent }

    var shouldContinue: Bool
    (shouldContinue, node.value) = walk(expr: node.value)
    return shouldContinue
  }

  public mutating func visit(_ node: AddrOfExpr) -> Bool {
    return traverse(node)
  }

  public mutating func traverse(_ node: AddrOfExpr) -> Bool {
    let prevParent = parent
    parent = node
    defer { parent = prevParent }

    var shouldContinue: Bool
    (shouldContinue, node.value) = walk(expr: node.value)
    return shouldContinue
  }

  public mutating func visit(_ node: MatchExpr) -> Bool {
    return traverse(node)
  }

  public mutating func traverse(_ node: MatchExpr) -> Bool {
    let prevParent = parent
    parent = node
    defer { parent = prevParent }

    var shouldContinue: Bool
    (shouldContinue, node.subject) = walk(expr: node.subject)
    guard shouldContinue else { return false }

    for i in 0 ..< node.cases.count {
      guard walk(stmt: node.cases[i]) else { return false }
    }

    return true
  }

  public mutating func visit(_ node: WildcardExpr) -> Bool {
    return true
  }

  public mutating func visit(_ node: ErrorExpr) -> Bool {
    return true
  }

  public mutating func visit(_ node: NamedPattern) -> Bool {
    return traverse(node)
  }

  public mutating func traverse(_ node: NamedPattern) -> Bool {
    let prevParent = parent
    parent = node
    defer { parent = prevParent }

    return walk(decl: node.decl)
  }

  public mutating func visit(_ node: TuplePattern) -> Bool {
    return traverse(node)
  }

  public mutating func traverse(_ node: TuplePattern) -> Bool {
    let prevParent = parent
    parent = node
    defer { parent = prevParent }

    var shouldContinue: Bool
    for i in 0 ..< node.elems.count {
      (shouldContinue, node.elems[i].pattern) = walk(pattern: node.elems[i].pattern)
      guard shouldContinue else { return false }
    }

    return true
  }

  public mutating func visit(_ node: BindingPattern) -> Bool {
    return traverse(node)
  }

  public mutating func traverse(_ node: BindingPattern) -> Bool {
    let prevParent = parent
    parent = node
    defer { parent = prevParent }

    var shouldContinue: Bool
    (shouldContinue, node.subpattern) = walk(pattern: node.subpattern)
    guard shouldContinue else { return false }

    if let signature = node.sign {
      (shouldContinue, node.sign) = walk(sign: signature)
      guard shouldContinue else { return false }
    }

    return true
  }

  public mutating func visit(_ node: WildcardPattern) -> Bool {
    return true
  }

  public mutating func visit(_ node: TupleSign) -> Bool {
    return traverse(node)
  }

  public mutating func traverse(_ node: TupleSign) -> Bool {
    let prevParent = parent
    parent = node
    defer { parent = prevParent }

    var shouldContinue: Bool
    for i in 0 ..< node.elems.count {
      (shouldContinue, node.elems[i].sign) = walk(sign: node.elems[i].sign)
      guard shouldContinue else { return false }
    }

    return true
  }

  public mutating func visit(_ node: FunSign) -> Bool {
    return traverse(node)
  }

  public mutating func traverse(_ node: FunSign) -> Bool {
    let prevParent = parent
    parent = node
    defer { parent = prevParent }

    var shouldContinue: Bool
    (shouldContinue, node.paramSign) = walk(sign: node.paramSign)
    guard shouldContinue else { return false }

    (shouldContinue, node.retSign) = walk(sign: node.retSign)
    guard shouldContinue else { return false }

    return true
  }

  public mutating func visit(_ node: AsyncSign) -> Bool {
    return traverse(node)
  }

  public mutating func traverse(_ node: AsyncSign) -> Bool {
    let prevParent = parent
    parent = node
    defer { parent = prevParent }

    var shouldContinue: Bool
    (shouldContinue, node.base) = walk(sign: node.base)
    return shouldContinue
  }

  public mutating func visit(_ node: InoutSign) -> Bool {
    return traverse(node)
  }

  public mutating func traverse(_ node: InoutSign) -> Bool {
    let prevParent = parent
    parent = node
    defer { parent = prevParent }

    var shouldContinue: Bool
    (shouldContinue, node.base) = walk(sign: node.base)
    return shouldContinue
  }

  public mutating func visit(_ node: UnionSign) -> Bool {
    return traverse(node)
  }

  public mutating func traverse(_ node: UnionSign) -> Bool {
    let prevParent = parent
    parent = node
    defer { parent = prevParent }

    var shouldContinue: Bool
    for i in 0 ..< node.elems.count {
      (shouldContinue, node.elems[i]) = walk(sign: node.elems[i])
      guard shouldContinue else { return false }
    }

    return true
  }

  public mutating func visit(_ node: ViewCompSign) -> Bool {
    return traverse(node)
  }

  public mutating func traverse(_ node: ViewCompSign) -> Bool {
    let prevParent = parent
    parent = node
    defer { parent = prevParent }

    var shouldContinue: Bool
    for i in 0 ..< node.views.count {
      (shouldContinue, node.views[i]) = walk(sign: node.views[i])
      guard shouldContinue else { return false }
    }

    return true
  }

  public mutating func visit(_ node: BareIdentSign) -> Bool {
    return true
  }

  public mutating func visit(_ node: SpecializedIdentSign) -> Bool {
    return traverse(node)
  }

  public mutating func traverse(_ node: SpecializedIdentSign) -> Bool {
    let prevParent = parent
    parent = node
    defer { parent = prevParent }

    var shouldContinue: Bool
    for i in 0 ..< node.args.count {
      (shouldContinue, node.args[i]) = walk(sign: node.args[i])
      guard shouldContinue else { return false }
    }

    return true
  }

  public mutating func visit(_ node: CompoundIdentSign) -> Bool {
    return traverse(node)
  }

  public mutating func traverse(_ node: CompoundIdentSign) -> Bool {
    let prevParent = parent
    parent = node
    defer { parent = prevParent }

    var shouldContinue: Bool
    for i in 0 ..< node.components.count {
      (shouldContinue, node.components[i]) = walk(sign: node.components[i])
        as! (Bool, IdentCompSign)
      guard shouldContinue else { return false }
    }

    return true
  }

  public mutating func visit(_ node: ErrorSign) -> Bool {
    return true
  }

  public mutating func visit(_ clause: GenericClause) -> Bool {
    return traverse(clause)
  }

  public mutating func traverse(_ clause: GenericClause) -> Bool {
    for i in 0 ..< clause.params.count {
      guard walk(decl: clause.params[i]) else { return false }
    }

    var shouldContinue: Bool
    for i in 0 ..< clause.typeReqs.count {
      (shouldContinue, clause.typeReqs[i].lhs) = walk(sign: clause.typeReqs[i].lhs)
        as! (Bool, IdentSign)
      guard shouldContinue else { return false }

      (shouldContinue, clause.typeReqs[i].rhs) = walk(sign: clause.typeReqs[i].rhs)
      guard shouldContinue else { return false }
    }

    return true
  }

}
