import AST
import Basic

struct TypeErrorReporter {

  unowned let context: AST.Context

  let solution: Solution

  func report(_ errors: [TypeError]) {
    // Sort the errors by source location.
    errors.sorted(by: <).forEach(report(_:))
  }

  func report(_ error: TypeError) {
    switch error {
    case .conflictingTypes(let constraint):
      let lhs = solution.reify(constraint.lhs, freeVariablePolicy: .keep)
      let rhs = solution.reify(constraint.rhs, freeVariablePolicy: .keep)

      // Compute the diagnostic's message.
      let message: String
      switch constraint.kind {
      case .equality:
        message = "type '\(lhs)' is not equal to type '\(rhs)'"
      case .conformance:
        message = "type '\(lhs)' does not conform to the view '\(rhs)'"
      case .subtyping:
        message = "type '\(lhs)' is not a subtype of type '\(rhs)'"
      case .conversion:
        message = "type '\(lhs)' is not expressible by type '\(rhs)' in conversion"
      }

      // Report the diagnostic.
      let anchor = constraint.locator.resolve()
      context.report(Diagnostic(message, anchor: anchor.range))

    case .nonConformingType(let constraint):
      let lhs = solution.reify(constraint.lhs, freeVariablePolicy: .keep)
      let rhs = solution.reify(constraint.rhs, freeVariablePolicy: .keep)
      assert(rhs is ViewType)

      let anchor = constraint.locator.resolve()
      context.report(
        Diagnostic("type '\(lhs)' does not conform to view '\(rhs)'", anchor: anchor.range))

    case .noViableOverload(let constraint):
      let message = "no viable overload to resolve '\(constraint.declSet[0].name)'"
      let anchor = constraint.locator.resolve()
      context.report(Diagnostic(message, anchor: anchor.range))

    case .multipleOverloads(let constraint, let decls):
      let message = "ambiguous use of '\(decls[0].name)'"
      let anchor = constraint.locator.resolve()
      context.report(Diagnostic(message, anchor: anchor.range))

    default:
      let anchor = error.constraint.locator.resolve()
      context.report(Diagnostic(String(describing: error), anchor: anchor.range))
    }
  }

}
