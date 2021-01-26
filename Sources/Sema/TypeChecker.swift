import AST

/// The type checker pass.
///
/// The static type checker verifies that program sources satisfy Val's static type system, i.e.,
/// that all of written expressions have an appropriate type for the context in which they occur.
///
/// The pass is actually a composition of three different steps:
/// 1. Constraint generation:
///    This step consists of walking the AST to generate type constraints from expressions.
/// 2. Constraint solving:
///    This step consists of solving the generated equations, through type inference.
/// 3. Solution application:
///    This pass consists of applying the solution computed by the previous pass to the AST.
public final class TypeChecker: AST.Pass {

  public static let name = "Type checker"

  public init(context: AST.Context) {
    self.context = context
  }

  /// The context in which the pass runs.
  public unowned let context: AST.Context

  public func run(on module: Module) throws {
    // Generate type constraints
    let generator = ConstraintGenerator(context: context)
    _ = generator.visit(module)

    // Sort the constraint system so that simpler constraints appear first.
    generator.system.sort()

    // Solve the constraint system.
    var solver = ConstraintSolver(
      system: generator.system,
      assumptions: SubstitutionTable(),
      penalities: 0,
      bestScore: .worst,
      context: context)
    let solution = solver.solve()

    // Report type errors.
    let reifier = TypeReifier(substitutions: solution.assumptions.flattened())
    let reporter = TypeErrorReporter(context: context, reifier: reifier)
    reporter.report(solution.errors)

    // Apply the solution.
    let dispatcher = TypeDispatcher(reifier: reifier)
    _ = dispatcher.visit(module)
  }

}
