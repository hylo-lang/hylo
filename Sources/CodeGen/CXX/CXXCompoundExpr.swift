import Core

/// A C++ compound expression; i.e., A.B
struct CXXCompoundExpr: CXXRepresentable {

  /// The base expression; i.e., the thing before the dot.
  public let base: CXXRepresentable

  /// The identifier of the compound expression; i.e., the thing after the dot.
  /// Typically this is a CXXIdentifier.
  public let id: CXXRepresentable

  /// The original node in Val AST.
  let original: AnyExprID.TypedNode?

  /// Construct the CXX compound expression from the base expression, identifier expression, and optionaly the original AST node.
  init<ID: NodeIDProtocol>(
    base: CXXRepresentable, id: CXXRepresentable, original: TypedNode<ID>? = nil
  ) {
    self.base = base
    self.id = id
    if let orig = original {
      self.original = orig as? AnyExprID.TypedNode
    } else {
      self.original = nil
    }
  }

  func writeCode<Target: TextOutputStream>(into target: inout Target) {
    base.writeCode(into: &target)
    target.write(".")
    id.writeCode(into: &target)
  }

}
