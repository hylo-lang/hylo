import AST
import Basic

/// A driver for constraint generation.
final class CSGenDriver: NodeWalker {

  init(checker: TypeChecker) {
    self.checker = checker
    super.init()
  }

  /// The type checker owning the driver.
  unowned let checker: TypeChecker

  /// The innermost declaration space in which the next expression will be visited.
  fileprivate var innermostSpace: DeclSpace?

  // MARK: AST Walk

  public override func willVisit(_ decl: Decl) -> (shouldWalk: Bool, nodeBefore: Decl) {
    if let space = decl as? DeclSpace {
      innermostSpace = space
    }

    if decl is FunParamDecl {
      // Parameters are visited via their function's realizer.
      return (true, decl)
    }

    return (decl.accept(DeclRealizer()), decl)
  }

  public override func didVisit(_ decl: Decl) -> (shouldContinue: Bool, nodeAfter: Decl) {
    if let space = decl as? DeclSpace {
      innermostSpace = space.parentDeclSpace
    }

    if let binding = decl as? PatternBindingDecl {
      ConstraintGenerator(checker: checker).visit(binding)
    }

    return (true, decl)
  }

  public override func willVisit(_ stmt: Stmt) -> (shouldWalk: Bool, nodeBefore: Stmt) {
    if let space = stmt as? DeclSpace {
      innermostSpace = space
    }
    return (true, stmt)
  }

  public override func didVisit(_ stmt: Stmt) -> (shouldContinue: Bool, nodeAfter: Stmt) {
    stmt.accept(ConstraintGenerator(checker: checker))

    if let space = stmt as? AbstractFunDecl {
      innermostSpace = space.parentDeclSpace
    }

    return (true, stmt)
  }

  public override func didVisit(_ expr: Expr) -> (shouldContinue: Bool, nodeAfter: Expr) {
    // Perform some pre-processing on the expression, in particular to bind and/or desugar
    // declaration references that could not be resolved by the parser.
    let newExpr = expr.accept(ExprBinder(space: innermostSpace!))

    // Generate type constraints.
    newExpr.accept(ConstraintGenerator(checker: checker))
    return (true, newExpr)
  }

  public override func willVisit(
    _ typeRepr: TypeRepr
  ) -> (shouldWalk: Bool, nodeBefore: TypeRepr) {
    typeRepr.realize(within: innermostSpace!)
    return (false, typeRepr)
  }

}
