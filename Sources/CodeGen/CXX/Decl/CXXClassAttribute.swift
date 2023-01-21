import Core

/// An attribute of a C++ class.
struct CXXClassAttribute: CXXDecl {

  /// The type of the attribute.
  let type: CXXTypeExpr
  /// The name of the attribute.
  let name: CXXIdentifier
  /// The initializer of the attribute.
  let initializer: CXXExpr?
  /// True if this is a static class attribute.
  let isStatic: Bool

  /// The original node in Val AST.
  let original: VarDecl.Typed

}
