import Core

/// A deferred type checking query on a node that should be applied after the types of its
/// constituent parts have been inferred.
typealias DeferredQuery = (_ checker: inout TypeChecker, _ solution: Solution) -> Bool
