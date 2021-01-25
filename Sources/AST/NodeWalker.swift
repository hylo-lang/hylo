/// A base class to implement AST "event-based" visitors and transformers.
open class NodeWalker: NodeVisitor {

  public typealias Result = Bool

  public init() {}

  /// The parent of the node being visited.
  public final var parent: Node?

  // MARK: Event handlers

  /// This method is called when the walker is about to visit a declaration.
  ///
  /// - Parameter decl: The declaration that will be visited.
  /// - Returns: `(flag, node)`, where `node` subsitutes the `expr` in the AST, and `flag` is a
  ///   Boolean value that indicates whether the walker should visit `node`. The default
  ///   implementation returns `(true, decl)`.
  open func willVisit(_ decl: Decl) -> (shouldWalk: Bool, nodeBefore: Decl) {
    return (true, decl)
  }

  /// This method is called after the walker visited a declaration.
  ///
  /// - Parameter decl: The declaration that was visited.
  /// - Returns: `(flag, node)` where `node` subsitutes the `expr` in the AST, and `flag` is a
  ///   Boolean value that indicates whether the walker should proceed to the next node. The
  ///   default implementation returns `(true, decl)`.
  open func didVisit(_ decl: Decl) -> (shouldContinue: Bool, nodeAfter: Decl) {
    return (true, decl)
  }

  /// This method is called when the walker is about to visit a statement.
  ///
  /// - Parameter stmt: The statement that will be visited.
  /// - Returns: `(flag, node)`, where `node` subsitutes the `stmt` in the AST, and `flag` is a
  ///   Boolean value that indicates whether the walker should visit `node`. The default
  ///   implementation returns `(true, stmt)`.
  open func willVisit(_ stmt: Stmt) -> (shouldWalk: Bool, nodeBefore: Stmt) {
    return (true, stmt)
  }

  /// This method is called after the walker visited a statement.
  ///
  /// - Parameter stmt: The statement that was visited.
  /// - Returns: `(flag, node)` where `node` subsitutes the `stmt` in the AST, and `flag` is a
  ///   Boolean value that indicates whether the walker should proceed to the next node. The
  ///   default implementation returns `(true, stmt)`.
  open func didVisit(_ stmt: Stmt) -> (shouldContinue: Bool, nodeAfter: Stmt) {
    return (true, stmt)
  }

  /// This method is called when the walker is about to visit an expression.
  ///
  /// - Parameter expr: The expression that will be visited.
  /// - Returns: `(flag, node)`, where `node` subsitutes the `expr` in the AST, and `flag` is a
  ///   Boolean value that indicates whether the walker should visit `node`. The default
  ///   implementation returns `(true, expr)`.
  open func willVisit(_ expr: Expr) -> (shouldWalk: Bool, nodeBefore: Expr) {
    return (true, expr)
  }

  /// This method is called after the walker visited an expression.
  ///
  /// - Parameter expr: The expression that was visited.
  /// - Returns: `(flag, node)` where `node` subsitutes the `expr` in the AST, and `flag` is a
  ///   Boolean value that indicates whether the walker should proceed to the next node. The
  ///   default implementation returns `(true, expr)`.
  open func didVisit(_ expr: Expr) -> (shouldContinue: Bool, nodeAfter: Expr) {
    return (true, expr)
  }

  /// This method is called when the walker is about to visit a pattern.
  ///
  /// - Parameter pattern: The patterm that will be visited.
  /// - Returns: `(flag, node)` where `node` subsitutes the `pattern` in the AST, and `flag` is a
  ///   Boolean value that indicates whether the walker should visit `node`. The default
  ///   implementation returns `(true, pattern)`.
  open func willVisit(_ pattern: Pattern) -> (shouldWalk: Bool, nodeBefore: Pattern) {
    return (true, pattern)
  }

  /// This method is called after the walker visited a pattern.
  ///
  /// - Parameter pattern: The patterm that was visited.
  /// - Returns: `(flag, node)` where `node` subsitutes the `pattern` in the AST, and `flag` is a
  ///   Boolean value that indicates whether the walker should proceed to the next node. The
  ///   default implementation returns `(true, pattern)`.
  open func didVisit(_ pattern: Pattern) -> (shouldContinue: Bool, nodeAfter: Pattern) {
    return (true, pattern)
  }

  /// This method is called when the walker is about to visit a type representation.
  ///
  /// - Parameter typeRepr: The type representation that will be visited.
  /// - Returns: `(flag, node)` where `node` subsitutes the `typeRepr` in the AST, and `flag` is a
  ///   Boolean value that indicates whether the walker should visit `node`. The default
  ///   implementation returns `(true, typeRepr)`.
  open func willVisit(_ typeRepr: TypeRepr) -> (shouldWalk: Bool, nodeBefore: TypeRepr) {
    return (true, typeRepr)
  }

  /// This method is called after the walker visited a type representation.
  ///
  /// - Parameter expr: The type representation that was visited.
  /// - Returns: `(flag, node)` where `node` subsitutes the `typeRepr` in the AST, and `flag` is a
  ///   Boolean value that indicates whether the walker should proceed to the next node. The
  ///   default implementation returns `(true, typeRepr)`.
  open func didVisit(_ typeRepr: TypeRepr) -> (shouldContinue: Bool, nodeAfter: TypeRepr) {
    return (true, typeRepr)
  }

  // MARK: Traversal

  private final var shouldContinue = true

  public final func visit(_ node: Module) -> Bool {
    let prevParent = parent
    parent = node
    defer { parent = prevParent }

    for i in 0 ..< node.statements.count {
      (shouldContinue, node.statements[i]) = process(node.statements[i])
      guard shouldContinue else { return false }
    }

    return true
  }

  public final func visit(_ node: PatternBindingDecl) -> Bool {
    let prevParent = parent
    parent = node
    defer { parent = prevParent }

    (shouldContinue, node.pattern) = process(node.pattern)

    guard shouldContinue else { return false }
    if let signature = node.typeSign {
      (shouldContinue, node.typeSign) = process(signature)
      guard shouldContinue else { return false }
    }

    if let initializer = node.initializer {
      (shouldContinue, node.initializer) = process(initializer)
      guard shouldContinue else { return false }
    }

    return true
  }

  public final func visit(_ node: VarDecl) -> Bool {
    return true
  }

  public final func visit(_ node: AbstractFunDecl) -> Bool {
    let prevParent = parent
    parent = node
    defer { parent = prevParent }

    for i in 0 ..< node.params.count {
      (shouldContinue, node.params[i]) = process(node.params[i]) as! (Bool, FunParamDecl)
      guard shouldContinue else { return false }
    }

    if let signature = node.retTypeSign {
      (shouldContinue, node.retTypeSign) = process(signature)
      guard shouldContinue else { return false }
    }

    if let body = node.body {
      (shouldContinue, node.body) = process(body) as! (Bool, BraceStmt)
      guard shouldContinue else { return false }
    }

    return true
  }

  public final func visit(_ node: FunDecl) -> Bool {
    return visit(node as AbstractFunDecl)
  }

  public final func visit(_ node: CtorDecl) -> Bool {
    return visit(node as AbstractFunDecl)
  }

  public final func visit(_ node: FunParamDecl) -> Bool {
    let prevParent = parent
    parent = node
    defer { parent = prevParent }

    if let signature = node.typeSign {
      (shouldContinue, node.typeSign) = process(signature)
      guard shouldContinue else { return false }
    }

    return true
  }

  public final func visit(_ node: AbstractNominalTypeDecl) -> Bool {
    let prevParent = parent
    parent = node
    defer { parent = prevParent }

    for i in 0 ..< node.inheritances.count {
      (shouldContinue, node.inheritances[i]) = process(node.inheritances[i])
      guard shouldContinue else { return false }
    }

    for i in 0 ..< node.members.count {
      (shouldContinue, node.members[i]) = process(node.members[i])
      guard shouldContinue else { return false }
    }

    return true
  }

  public final func visit(_ node: ProductTypeDecl) -> Bool {
    return visit(node as AbstractNominalTypeDecl)
  }

  public final func visit(_ node: ViewTypeDecl) -> Bool {
    return visit(node as AbstractNominalTypeDecl)
  }

  public final func visit(_ node: TypeExtDecl) -> Bool {
    let prevParent = parent
    parent = node
    defer { parent = prevParent }

    (shouldContinue, node.extendedIdent) = process(node.extendedIdent) as! (Bool, IdentTypeRepr)
    guard shouldContinue else { return false }

    for i in 0 ..< node.members.count {
      (shouldContinue, node.members[i]) = process(node.members[i])
      guard shouldContinue else { return false }
    }

    return true
  }

  public final func visit(_ node: BraceStmt) -> Bool {
    let prevParent = parent
    parent = node
    defer { parent = prevParent }

    for i in 0 ..< node.statements.count {
      (shouldContinue, node.statements[i]) = process(node.statements[i])
      guard shouldContinue else { return false }
    }

    return true
  }

  public final func visit(_ node: RetStmt) -> Bool {
    let prevParent = parent
    parent = node
    defer { parent = prevParent }

    if let value = node.value {
      (shouldContinue, node.value) = process(value)
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

    (shouldContinue, node.lvalue) = process(node.lvalue)
    guard shouldContinue else { return false }

    (shouldContinue, node.rvalue) = process(node.rvalue)
    guard shouldContinue else { return false }

    return true
  }

  public final func visit(_ node: CallExpr) -> Bool {
    let prevParent = parent
    parent = node
    defer { parent = prevParent }

    (shouldContinue, node.fun) = process(node.fun)
    guard shouldContinue else { return false }

    for i in 0 ..< node.args.count {
      (shouldContinue, node.args[i].value) = process(node.args[i].value)
      guard shouldContinue else { return false }
    }

    return true
  }

  public final func visit(_ node: UnresolvedDeclRefExpr) -> Bool {
    return true
  }

  public final func visit(_ node: QualifiedDeclRefExpr) -> Bool {
    let prevParent = parent
    parent = node
    defer { parent = prevParent }

    (shouldContinue, node.namespace) = process(node.namespace) as! (Bool, IdentTypeRepr)
    guard shouldContinue else { return false }

    return true
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

    (shouldContinue, node.base) = process(node.base)
    guard shouldContinue else { return false }

    return true
  }

  public final func visit(_ node: MemberRefExpr) -> Bool {
    let prevParent = parent
    parent = node
    defer { parent = prevParent }

    (shouldContinue, node.base) = process(node.base)
    guard shouldContinue else { return false }

    return true
  }

  public final func visit(_ node: AddrOfExpr) -> Bool {
    let prevParent = parent
    parent = node
    defer { parent = prevParent }

    (shouldContinue, node.value) = process(node.value)
    guard shouldContinue else { return false }

    return true
  }

  public final func visit(_ node: WildcardExpr) -> Bool {
    return true
  }

  public final func visit(_ node: NamedPattern) -> Bool {
    let prevParent = parent
    parent = node
    defer { parent = prevParent }

    (shouldContinue, node.decl) = process(node.decl) as! (Bool, VarDecl)
    guard shouldContinue else { return false }

    return true
  }

  public final func visit(_ node: TuplePattern) -> Bool {
    let prevParent = parent
    parent = node
    defer { parent = prevParent }

    for i in 0 ..< node.elems.count {
      (shouldContinue, node.elems[i].pattern) = process(node.elems[i].pattern)
      guard shouldContinue else { return false }
    }

    return true
  }

  public final func visit(_ node: WildcardPattern) -> Bool {
    return true
  }

  public final func visit(_ node: BuiltinTypeRepr) -> Bool {
    return true
  }

  public final func visit(_ node: UnqualTypeRepr) -> Bool {
    return true
  }

  public final func visit(_ node: CompoundTypeRepr) -> Bool {
    let prevParent = parent
    parent = node
    defer { parent = prevParent }

    for i in 0 ..< node.components.count {
      (shouldContinue, node.components[i]) = process(node.components[i]) as! (Bool, UnqualTypeRepr)
      guard shouldContinue else { return false }
    }

    return true
  }

  private final func process(_ decl: Decl) -> (Bool, Decl) {
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

  private final func process(_ stmt: Stmt) -> (Bool, Stmt) {
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

  private final func process(_ expr: Expr) -> (Bool, Expr) {
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

  private final func process(_ pattern: Pattern) -> (Bool, Pattern) {
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

  private final func process(_ typeRepr: TypeRepr) -> (Bool, TypeRepr) {
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

  private final func process(_ node: Node) -> (Bool, Node) {
    switch node {
    case let d as Decl    : return process(d)
    case let s as Stmt    : return process(s)
    case let e as Expr    : return process(e)
    case let p as Pattern : return process(p)
    case let t as TypeRepr: return process(t)
    default:
      fatalError("unreachable")
    }
  }

}
