import Core
import Utils

/// A deferred type checking query that should be executed by applying `execute` on `subject` the
/// type of its constituent parts have been inferred.
struct DeferredQuery<I: NodeIDProtocol> {

  /// A function that uses `checker` to type check `subject` using with the inference results
  /// stored in `solution`.
  typealias Check = (_ checker: inout TypeChecker, _ subject: I, _ solution: Solution) -> Bool

  /// A function implementing the execution of the query.
  let execute: Check

  /// The identity of the root node on which the query applies.
  let subject: I

  /// Creates an instance applying `f` on `s` when executed.
  init(on s: I, executedWith f: @escaping Check) {
    self.subject = s
    self.execute = f
  }

  /// Creates a type-erased container wrapping the given instance.
  static prefix func ^ (_ q: Self) -> AnyDeferredQuery {
    AnyDeferredQuery(q)
  }

}
