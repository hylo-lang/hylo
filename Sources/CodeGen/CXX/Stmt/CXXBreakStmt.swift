import Core

/// A C++ `break` statement
struct CXXBreakStmt: CXXStmt {

  /// The original node in Val AST.
  /// This node can be of any type.
  let original: AnyNodeID.TypedNode?

}
