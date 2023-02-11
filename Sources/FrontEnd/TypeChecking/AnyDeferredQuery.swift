import Utils

/// A type-erased deferred type checking query.
typealias AnyDeferredQuery = (_ checker: inout TypeChecker, _ solution: Solution) -> Bool

extension DeferredQuery {

  /// Returns a partial application of `q.execute`, capturing `q.subject`.
  static prefix func ^ (_ q: Self) -> AnyDeferredQuery {
    { (c, s) in q.execute(&c, q.subject, s) }
  }

}
