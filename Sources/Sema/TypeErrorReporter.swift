import AST
import Basic

struct TypeErrorReporter {

  unowned let context: AST.Context

  let reifier: TypeReifier

  func report(_ errors: [TypeError]) {
    // Sort the errors by source location.
    errors.sorted(by: <).forEach(report(_:))
  }

  func report(_ error: TypeError) {
    switch error {
    case .conflictingTypes(let constraint):
      let lhs = constraint.lhs.accept(reifier)
      let rhs = constraint.rhs.accept(reifier)

      // Compute the diagnostic's message.
      let message: String
      switch constraint {
      case is SubtypingConstraint:
        message = "type '\(lhs)' is not a subtype of to type '\(rhs)'"
      default:
        message = "type '\(lhs)' is not equal to type '\(rhs)'"
      }

      // Compute the diagnostic's location.
      let anchor = constraint.locator?.resolve()
      context.report(Diagnostic(message, anchor: anchor?.range))

    default:
      let anchor =  error.constraint.locator?.resolve()
      context.report(Diagnostic(String(describing: error), anchor: anchor?.range))
    }
  }

}
