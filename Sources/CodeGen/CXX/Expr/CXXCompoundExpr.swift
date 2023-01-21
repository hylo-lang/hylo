import Core

/// A C++ compound expression; i.e., A.B
struct CXXCompoundExpr: CXXRepresentable {

  /// The base expression; i.e., the thing before the dot.
  public let base: CXXNode

  /// The identifier of the compound expression; i.e., the thing after the dot.
  /// Typically this is a CXXIdentifier.
  public let id: CXXNode

  /// The original node in Val AST.
  let original: AnyExprID.TypedNode?

  func writeCode<Target: TextOutputStream>(into target: inout Target) {
    base.writeCode(into: &target)
    target.write(".")
    id.writeCode(into: &target)
  }

}
