/// A base class to implement "event-based" AST visitors and transformers.
open class NodeWalker: NodeVisitor {

  public typealias Result = Bool

  public init(parent: Node? = nil, innermostSpace: DeclSpace? = nil) {
    self.parent = parent
    self.innermostSpace = innermostSpace
  }

  /// The parent of the node being visited.
  public final private(set) var parent: Node?

  /// The innermost declaration space in which the next node will be visited.
  public final private(set) var innermostSpace: DeclSpace?

  // MARK: Event handlers

  /// This method is called when the walker is about to visit a declaration.
  ///
  /// - Parameter decl: The declaration that will be visited.
  /// - Returns: `(flag, node)`, where `node` subsitutes `expr` in the AST, and `flag` is a
  ///   Boolean value that indicates whether the walker should visit `node`. The default
  ///   implementation returns `(true, decl)`.
  open func willVisit(_ decl: Decl) -> (shouldWalk: Bool, nodeBefore: Decl) {
    return (true, decl)
  }

  /// This method is called after the walker visited a declaration.
  ///
  /// - Parameter decl: The declaration that was visited.
  /// - Returns: `(flag, node)` where `node` subsitutes `expr` in the AST, and `flag` is a
  ///   Boolean value that indicates whether the walker should proceed to the next node. The
  ///   default implementation returns `(true, decl)`.
  open func didVisit(_ decl: Decl) -> (shouldContinue: Bool, nodeAfter: Decl) {
    return (true, decl)
  }

  /// This method is called when the walker is about to visit a statement.
  ///
  /// - Parameter stmt: The statement that will be visited.
  /// - Returns: `(flag, node)`, where `node` subsitutes `stmt` in the AST, and `flag` is a
  ///   Boolean value that indicates whether the walker should visit `node`. The default
  ///   implementation returns `(true, stmt)`.
  open func willVisit(_ stmt: Stmt) -> (shouldWalk: Bool, nodeBefore: Stmt) {
    return (true, stmt)
  }

  /// This method is called after the walker visited a statement.
  ///
  /// - Parameter stmt: The statement that was visited.
  /// - Returns: `(flag, node)` where `node` subsitutes `stmt` in the AST, and `flag` is a
  ///   Boolean value that indicates whether the walker should proceed to the next node. The
  ///   default implementation returns `(true, stmt)`.
  open func didVisit(_ stmt: Stmt) -> (shouldContinue: Bool, nodeAfter: Stmt) {
    return (true, stmt)
  }

  /// This method is called when the walker is about to visit an expression.
  ///
  /// - Parameter expr: The expression that will be visited.
  /// - Returns: `(flag, node)`, where `node` subsitutes `expr` in the AST, and `flag` is a
  ///   Boolean value that indicates whether the walker should visit `node`. The default
  ///   implementation returns `(true, expr)`.
  open func willVisit(_ expr: Expr) -> (shouldWalk: Bool, nodeBefore: Expr) {
    return (true, expr)
  }

  /// This method is called after the walker visited an expression.
  ///
  /// - Parameter expr: The expression that was visited.
  /// - Returns: `(flag, node)` where `node` subsitutes `expr` in the AST, and `flag` is a
  ///   Boolean value that indicates whether the walker should proceed to the next node. The
  ///   default implementation returns `(true, expr)`.
  open func didVisit(_ expr: Expr) -> (shouldContinue: Bool, nodeAfter: Expr) {
    return (true, expr)
  }

  /// This method is called when the walker is about to visit a pattern.
  ///
  /// - Parameter pattern: The patterm that will be visited.
  /// - Returns: `(flag, node)` where `node` subsitutes `pattern` in the AST, and `flag` is a
  ///   Boolean value that indicates whether the walker should visit `node`. The default
  ///   implementation returns `(true, pattern)`.
  open func willVisit(_ pattern: Pattern) -> (shouldWalk: Bool, nodeBefore: Pattern) {
    return (true, pattern)
  }

  /// This method is called after the walker visited a pattern.
  ///
  /// - Parameter pattern: The patterm that was visited.
  /// - Returns: `(flag, node)` where `node` subsitutes `pattern` in the AST, and `flag` is a
  ///   Boolean value that indicates whether the walker should proceed to the next node. The
  ///   default implementation returns `(true, pattern)`.
  open func didVisit(_ pattern: Pattern) -> (shouldContinue: Bool, nodeAfter: Pattern) {
    return (true, pattern)
  }

  /// This method is called when the walker is about to visit a type representation.
  ///
  /// - Parameter typeRepr: The type representation that will be visited.
  /// - Returns: `(flag, node)` where `node` subsitutes `typeRepr` in the AST, and `flag` is a
  ///   Boolean value that indicates whether the walker should visit `node`. The default
  ///   implementation returns `(true, typeRepr)`.
  open func willVisit(_ typeRepr: TypeRepr) -> (shouldWalk: Bool, nodeBefore: TypeRepr) {
    return (true, typeRepr)
  }

  /// This method is called after the walker visited a type representation.
  ///
  /// - Parameter expr: The type representation that was visited.
  /// - Returns: `(flag, node)` where `node` subsitutes `typeRepr` in the AST, and `flag` is a
  ///   Boolean value that indicates whether the walker should proceed to the next node. The
  ///   default implementation returns `(true, typeRepr)`.
  open func didVisit(_ typeRepr: TypeRepr) -> (shouldContinue: Bool, nodeAfter: TypeRepr) {
    return (true, typeRepr)
  }

  /// This method is called when the walker is about to visit a generic clause.
  ///
  /// - Parameter clause: The generic clause that will be visited.
  /// - Returns: `true` if the walker should visit the clause, or `false` if it should skip it. The
  ///   default implementation returns `true`.
  open func willVisit(_ clause: GenericClause) -> Bool {
    return true
  }

  /// This method is called after the walker visited a generic clause.
  ///
  /// - Parameter clause: The generic clause that was visited.
  /// - Returns: `true` if the walker should proceed to the next node, or `false` otherwise. The
  ///   default implementation returns `true`.
  open func didVisit(_ clause: GenericClause) -> Bool {
    return true
  }

  // MARK: Traversal

  private final var shouldContinue = true

  public final func visit(_ node: ModuleDecl) -> Bool {
    let prevParent = parent
    parent = node
    innermostSpace = node
    defer {
      parent = prevParent
      innermostSpace = nil
    }

    for i in node.indices {
      (shouldContinue, node[i]) = walk(node[i])
      guard shouldContinue else { return false }
    }

    return true
  }

  public final func visit(_ node: ImportDecl) -> Bool {
    let prevParent = parent
    parent = node
    defer {
      parent = prevParent
    }

    return true
  }

  public final func visit(_ node: PatternBindingDecl) -> Bool {
    let prevParent = parent
    parent = node
    defer { parent = prevParent }

    (shouldContinue, node.pattern) = walk(node.pattern)

    guard shouldContinue else { return false }
    if let signature = node.sign {
      (shouldContinue, node.sign) = walk(signature)
      guard shouldContinue else { return false }
    }

    if let initializer = node.initializer {
      (shouldContinue, node.initializer) = walk(initializer)
      guard shouldContinue else { return false }
    }

    return true
  }

  public final func visit(_ node: VarDecl) -> Bool {
    return true
  }

  public final func visit(_ node: BaseFunDecl) -> Bool {
    let prevParent = parent
    parent = node
    innermostSpace = node
    defer {
      parent = prevParent
      innermostSpace = innermostSpace?.parentDeclSpace
    }

    if let clause = node.genericClause, willVisit(clause) {
      guard visit(clause)    else { return false }
      guard didVisit(clause) else { return false }
    }

    for i in 0 ..< node.params.count {
      (shouldContinue, node.params[i]) = walk(node.params[i]) as! (Bool, FunParamDecl)
      guard shouldContinue else { return false }
    }

    if let signature = node.retSign {
      (shouldContinue, node.retSign) = walk(signature)
      guard shouldContinue else { return false }
    }

    if let body = node.body {
      (shouldContinue, node.body) = walk(body) as! (Bool, BraceStmt)
      guard shouldContinue else { return false }
    }

    return true
  }

  public final func visit(_ node: FunDecl) -> Bool {
    return visit(node as BaseFunDecl)
  }

  public final func visit(_ node: CtorDecl) -> Bool {
    return visit(node as BaseFunDecl)
  }

  public final func visit(_ node: FunParamDecl) -> Bool {
    let prevParent = parent
    parent = node
    defer { parent = prevParent }

    if let signature = node.sign {
      (shouldContinue, node.sign) = walk(signature)
      guard shouldContinue else { return false }
    }

    return true
  }

  public final func visit(_ node: GenericTypeDecl) -> Bool {
    switch node {
    case let decl as NominalTypeDecl: return visit(decl)
    case let decl as AliasTypeDecl  : return visit(decl)
    default: fatalError("unreachable")
    }
  }

  public final func visit(_ node: NominalTypeDecl) -> Bool {
    switch node {
    case let decl as ProductTypeDecl: return visit(decl)
    case let decl as ViewTypeDecl   : return visit(decl)
    default: fatalError("unreachable")
    }
  }

  public final func visit(_ node: ProductTypeDecl) -> Bool {
    let prevParent = parent
    parent = node
    innermostSpace = node
    defer {
      parent = prevParent
      innermostSpace = innermostSpace?.parentDeclSpace
    }

    if let clause = node.genericClause, willVisit(clause) {
      guard visit(clause)    else { return false }
      guard didVisit(clause) else { return false }
    }

    for i in 0 ..< node.inheritances.count {
      (shouldContinue, node.inheritances[i]) = walk(node.inheritances[i])
      guard shouldContinue else { return false }
    }

    for i in 0 ..< node.members.count {
      (shouldContinue, node.members[i]) = walk(node.members[i])
      guard shouldContinue else { return false }
    }

    return true
  }

  public final func visit(_ node: ViewTypeDecl) -> Bool {
    let prevParent = parent
    parent = node
    innermostSpace = node
    defer {
      parent = prevParent
      innermostSpace = innermostSpace?.parentDeclSpace
    }

    for i in 0 ..< node.inheritances.count {
      (shouldContinue, node.inheritances[i]) = walk(node.inheritances[i])
      guard shouldContinue else { return false }
    }

    for i in 0 ..< node.members.count {
      (shouldContinue, node.members[i]) = walk(node.members[i])
      guard shouldContinue else { return false }
    }

    return true
  }

  public final func visit(_ node: AliasTypeDecl) -> Bool {
    let prevParent = parent
    parent = node
    innermostSpace = node
    defer {
      parent = prevParent
      innermostSpace = innermostSpace?.parentDeclSpace
    }

//    if let clause = node.genericClause, willVisit(clause) {
//      guard visit(clause)    else { return false }
//      guard didVisit(clause) else { return false }
//    }
//
//    for i in 0 ..< node.inheritances.count {
//      (shouldContinue, node.inheritances[i]) = walk(node.inheritances[i])
//      guard shouldContinue else { return false }
//    }

    (shouldContinue, node.aliasedSign) = walk(node.aliasedSign)
    guard shouldContinue else { return false }

    return true
  }

  public final func visit(_ node: GenericParamDecl) -> Bool {
    return true
  }

  public final func visit(_ node: TypeExtDecl) -> Bool {
    let prevParent = parent
    parent = node
    innermostSpace = node
    defer {
      parent = prevParent
      innermostSpace = innermostSpace?.parentDeclSpace
    }

    (shouldContinue, node.extendedIdent) = walk(node.extendedIdent) as! (Bool, IdentTypeRepr)
    guard shouldContinue else { return false }

    for i in 0 ..< node.members.count {
      (shouldContinue, node.members[i]) = walk(node.members[i])
      guard shouldContinue else { return false }
    }

    return true
  }

  public final func visit(_ node: BraceStmt) -> Bool {
    let prevParent = parent
    parent = node
    innermostSpace = node
    defer {
      parent = prevParent
      innermostSpace = innermostSpace?.parentDeclSpace
    }

    for i in 0 ..< node.stmts.count {
      (shouldContinue, node.stmts[i]) = walk(node.stmts[i])
      guard shouldContinue else { return false }
    }

    return true
  }

  public final func visit(_ node: RetStmt) -> Bool {
    let prevParent = parent
    parent = node
    defer { parent = prevParent }

    if let value = node.value {
      (shouldContinue, node.value) = walk(value)
      guard shouldContinue else { return false }
    }

    return true
  }

  public final func visit(_ node: IntLiteralExpr) -> Bool {
    return true
  }

  public final func visit(_ node: AssignExpr) -> Bool {
    let prevParent = parent
    parent = node
    defer { parent = prevParent }

    (shouldContinue, node.lvalue) = walk(node.lvalue)
    guard shouldContinue else { return false }

    (shouldContinue, node.rvalue) = walk(node.rvalue)
    guard shouldContinue else { return false }

    return true
  }

  public final func visit(_ node: UnsafeCastExpr) -> Bool {
    let prevParent = parent
    parent = node
    defer { parent = prevParent }

    (shouldContinue, node.value) = walk(node.value)
    guard shouldContinue else { return false }

    (shouldContinue, node.sign) = walk(node.sign)
    guard shouldContinue else { return false }

    return true
  }

  public final func visit(_ node: TupleExpr) -> Bool {
    let prevParent = parent
    parent = node
    defer { parent = prevParent }

    for i in 0 ..< node.elems.count {
      (shouldContinue, node.elems[i].value) = walk(node.elems[i].value)
      guard shouldContinue else { return false }
    }

    return true
  }

  public final func visit(_ node: CallExpr) -> Bool {
    let prevParent = parent
    parent = node
    defer { parent = prevParent }

    (shouldContinue, node.fun) = walk(node.fun)
    guard shouldContinue else { return false }

    for i in 0 ..< node.args.count {
      (shouldContinue, node.args[i].value) = walk(node.args[i].value)
      guard shouldContinue else { return false }
    }

    return true
  }

  public final func visit(_ node: UnresolvedDeclRefExpr) -> Bool {
    return true
  }

  public final func visit(_ node: UnresolvedQualDeclRefExpr) -> Bool {
    let prevParent = parent
    parent = node
    defer { parent = prevParent }

    (shouldContinue, node.namespace) = walk(node.namespace) as! (Bool, IdentTypeRepr)
    return shouldContinue
  }

  public final func visit(_ node: OverloadedDeclRefExpr) -> Bool {
    return true
  }

  public final func visit(_ node: DeclRefExpr) -> Bool {
    return true
  }

  public final func visit(_ node: TypeDeclRefExpr) -> Bool {
    return true
  }

  public final func visit(_ node: UnresolvedMemberExpr) -> Bool {
    let prevParent = parent
    parent = node
    defer { parent = prevParent }

    (shouldContinue, node.base) = walk(node.base)
    return shouldContinue
  }

  public final func visit(_ node: MemberDeclRefExpr) -> Bool {
    let prevParent = parent
    parent = node
    defer { parent = prevParent }

    (shouldContinue, node.base) = walk(node.base)
    return shouldContinue
  }

  public final func visit(_ node: TupleMemberExpr) -> Bool {
    let prevParent = parent
    parent = node
    defer { parent = prevParent }

    (shouldContinue, node.base) = walk(node.base)
    return shouldContinue
  }

  public final func visit(_ node: AsyncExpr) -> Bool {
    let prevParent = parent
    parent = node
    defer { parent = prevParent }

    (shouldContinue, node.value) = walk(node.value)
    return shouldContinue
  }

  public final func visit(_ node: AwaitExpr) -> Bool {
    let prevParent = parent
    parent = node
    defer { parent = prevParent }

    (shouldContinue, node.value) = walk(node.value)
    return shouldContinue
  }

  public final func visit(_ node: AddrOfExpr) -> Bool {
    let prevParent = parent
    parent = node
    defer { parent = prevParent }

    (shouldContinue, node.value) = walk(node.value)
    return shouldContinue
  }

  public final func visit(_ node: WildcardExpr) -> Bool {
    return true
  }

  public final func visit(_ node: ErrorExpr) -> Bool {
    return true
  }

  public final func visit(_ node: NamedPattern) -> Bool {
    let prevParent = parent
    parent = node
    defer { parent = prevParent }

    (shouldContinue, node.decl) = walk(node.decl) as! (Bool, VarDecl)
    return shouldContinue
  }

  public final func visit(_ node: TuplePattern) -> Bool {
    let prevParent = parent
    parent = node
    defer { parent = prevParent }

    for i in 0 ..< node.elems.count {
      (shouldContinue, node.elems[i].pattern) = walk(node.elems[i].pattern)
      guard shouldContinue else { return false }
    }

    return true
  }

  public final func visit(_ node: WildcardPattern) -> Bool {
    return true
  }

  public final func visit(_ node: TupleTypeRepr) -> Bool {
    let prevParent = parent
    parent = node
    defer { parent = prevParent }

    for i in 0 ..< node.elems.count {
      (shouldContinue, node.elems[i].sign) = walk(node.elems[i].sign)
      guard shouldContinue else { return false }
    }

    return true
  }

  public final func visit(_ node: FunTypeRepr) -> Bool {
    let prevParent = parent
    parent = node
    defer { parent = prevParent }

    (shouldContinue, node.paramSign) = walk(node.paramSign)
    guard shouldContinue else { return false }

    (shouldContinue, node.retSign) = walk(node.retSign)
    guard shouldContinue else { return false }

    return true
  }

  public final func visit(_ node: AsyncTypeRepr) -> Bool {
    let prevParent = parent
    parent = node
    defer { parent = prevParent }

    (shouldContinue, node.base) = walk(node.base)
    return shouldContinue
  }

  public final func visit(_ node: InoutTypeRepr) -> Bool {
    let prevParent = parent
    parent = node
    defer { parent = prevParent }

    (shouldContinue, node.base) = walk(node.base)
    return shouldContinue
  }

  public final func visit(_ node: UnionTypeRepr) -> Bool {
    let prevParent = parent
    parent = node
    defer { parent = prevParent }

    for i in 0 ..< node.elems.count {
      (shouldContinue, node.elems[i]) = walk(node.elems[i])
      guard shouldContinue else { return false }
    }

    return true
  }

  public final func visit(_ node: ViewCompTypeRepr) -> Bool {
    let prevParent = parent
    parent = node
    defer { parent = prevParent }

    for i in 0 ..< node.views.count {
      (shouldContinue, node.views[i]) = walk(node.views[i])
      guard shouldContinue else { return false }
    }

    return true
  }

  public final func visit(_ node: UnqualTypeRepr) -> Bool {
    return true
  }

  public final func visit(_ node: SpecializedTypeRepr) -> Bool {
    let prevParent = parent
    parent = node
    defer { parent = prevParent }

    for i in 0 ..< node.args.count {
      (shouldContinue, node.args[i]) = walk(node.args[i])
      guard shouldContinue else { return false }
    }

    return true
  }

  public final func visit(_ node: CompoundTypeRepr) -> Bool {
    let prevParent = parent
    parent = node
    defer { parent = prevParent }

    for i in 0 ..< node.components.count {
      (shouldContinue, node.components[i]) = walk(node.components[i]) as! (Bool, ComponentTypeRepr)
      guard shouldContinue else { return false }
    }

    return true
  }

  public final func visit(_ clause: GenericClause) -> Bool {
    for i in 0 ..< clause.params.count {
      (shouldContinue, clause.params[i]) = walk(clause.params[i])
        as! (Bool, GenericParamDecl)
      guard shouldContinue else { return false }
    }

    for i in 0 ..< clause.typeReqs.count {
      (shouldContinue, clause.typeReqs[i].lhs) = walk(clause.typeReqs[i].lhs)
        as! (Bool, IdentTypeRepr)
      guard shouldContinue else { return false }

      (shouldContinue, clause.typeReqs[i].rhs) = walk(clause.typeReqs[i].rhs)
      guard shouldContinue else { return false }
    }

    return true
  }

  public final func walk(_ decl: Decl) -> (Bool, Decl) {
    // Fire the `willVisit` event.
    let (shouldVisit, substitute) = willVisit(decl)
    guard shouldVisit else {
      return (true, substitute)
    }

    // Visit the node's children.
    guard substitute.accept(self) else {
      return (false, substitute)
    }

    // Fire the `didVisit` event.
    return didVisit(substitute)
  }

  public final func walk(_ stmt: Stmt) -> (Bool, Stmt) {
    // Fire the `willVisit` event.
    let (shouldVisit, substitute) = willVisit(stmt)
    guard shouldVisit else {
      return (true, substitute)
    }

    // Visit the node's children.
    guard substitute.accept(self) else {
      return (false, substitute)
    }

    // Fire the `didVisit` event.
    return didVisit(substitute)
  }

  public final func walk(_ expr: Expr) -> (Bool, Expr) {
    // Fire the `willVisit` event.
    let (shouldVisit, substitute) = willVisit(expr)
    guard shouldVisit else {
      return (true, substitute)
    }

    // Visit the node's children.
    guard substitute.accept(self) else {
      return (false, substitute)
    }

    // Fire the `didVisit` event.
    return didVisit(substitute)
  }

  public final func walk(_ pattern: Pattern) -> (Bool, Pattern) {
    // Fire the `willVisit` event.
    let (shouldVisit, substitute) = willVisit(pattern)
    guard shouldVisit else {
      return (true, substitute)
    }

    // Visit the node's children.
    guard substitute.accept(self) else {
      return (false, substitute)
    }

    // Fire the `didVisit` event.
    return didVisit(substitute)
  }

  public final func walk(_ typeRepr: TypeRepr) -> (Bool, TypeRepr) {
    // Fire the `willVisit` event.
    let (shouldVisit, substitute) = willVisit(typeRepr)
    guard shouldVisit else {
      return (true, substitute)
    }

    // Visit the node's children.
    guard substitute.accept(self) else {
      return (false, substitute)
    }

    // Fire the `didVisit` event.
    return didVisit(substitute)
  }

  public final func walk(_ node: Node) -> (Bool, Node) {
    switch node {
    case let d as Decl    : return walk(d)
    case let s as Stmt    : return walk(s)
    case let e as Expr    : return walk(e)
    case let p as Pattern : return walk(p)
    case let t as TypeRepr: return walk(t)
    default: fatalError("unreachable")
    }
  }

}
