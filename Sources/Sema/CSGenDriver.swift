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

  // MARK: AST Walk

  public override func willVisit(_ decl: Decl) -> (shouldWalk: Bool, nodeBefore: Decl) {
    // Bind extensions to the type they extend.
    if let ext = decl as? TypeExtDecl {
      _ = ext.bind()
    }

    // Realize value declarations.
    if let valueDecl = decl as? ValueDecl {
      _ = valueDecl.realize()
    }

    return (true, decl)
  }

  public override func didVisit(_ decl: Decl) -> (shouldContinue: Bool, nodeAfter: Decl) {
    // Generate the type constraints related to the declaration's semantic validation.
    if let binding = decl as? PatternBindingDecl {
      ConstraintGenerator(checker: checker).visit(binding)
    }

    return (true, decl)
  }

  public override func didVisit(_ stmt: Stmt) -> (shouldContinue: Bool, nodeAfter: Stmt) {
    // Generate type constraints for the statement.
    stmt.accept(ConstraintGenerator(checker: checker))
    return (true, stmt)
  }

  public override func didVisit(_ expr: Expr) -> (shouldContinue: Bool, nodeAfter: Expr) {
    // Perform some pre-processing on the expression, in particular to bind and/or desugar
    // declaration references that could not be resolved by the parser.
    let newExpr = expr.accept(ExprBinder(space: innermostSpace!))
    guard !(newExpr is ErrorExpr) else {
      // Skip error ill-formed expressions.
      return (false, newExpr)
    }

    // Generate type constraints for the expression.
    newExpr.accept(ConstraintGenerator(checker: checker))
    return (true, newExpr)
  }

  public override func willVisit(
    _ typeRepr: TypeRepr
  ) -> (shouldWalk: Bool, nodeBefore: TypeRepr) {
    if typeRepr is IdentTypeRepr {
      // Ident type representations must be visited *before* walking their children, as we need
      // access to the entire component list to perform qualified lookups.
      typeRepr.realize(unqualifiedFrom: innermostSpace!)
      return (false, typeRepr)
    }

    return (false, typeRepr)
  }

  public override func didVisit(
    _ typeRepr: TypeRepr
  ) -> (shouldContinue: Bool, nodeAfter: TypeRepr) {
    typeRepr.realize(unqualifiedFrom: innermostSpace!)
    return (true, typeRepr)
  }

}
