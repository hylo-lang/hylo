/// A box wrapping a deferred type checking query.
private protocol DeferredQueryBox {

  /// Executes this query using `checker` and reading inference results from `solution`.
  func _execute(_ checker: inout TypeChecker, _ solution: Solution) -> Bool

}

extension DeferredQuery: DeferredQueryBox {

  func _execute(_ checker: inout TypeChecker, _ solution: Solution) -> Bool {
    execute(&checker, subject, solution)
  }

}

// A type-erased deferred type checking query.
struct AnyDeferredQuery {

  /// The value wrapped by this instance.
  private let wrapped: DeferredQueryBox

  /// Creates a type-erased container wrapping `q`.
  init<I>(_ q: DeferredQuery<I>) {
    self.wrapped = q
  }

  /// Executes this query using `checker` and reading inference results from `solution`.
  func execute(_ checker: inout TypeChecker, _ solution: Solution) -> Bool {
    wrapped._execute(&checker, solution)
  }

}
