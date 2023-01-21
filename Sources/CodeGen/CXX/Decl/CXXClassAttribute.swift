import Core

/// An attribute of a C++ class.
public struct CXXClassAttribute: CXXRepresentable {

  /// The type of the attribute.
  public let type: CXXTypeExpr
  /// The name of the attribute.
  public let name: CXXIdentifier
  /// The initializer of the attribute.
  public let initializer: CXXNode?
  /// True if this is a static class attribute.
  public let isStatic: Bool

  /// The original node in Val AST.
  let original: VarDecl.Typed

  public func writeCode<Target: TextOutputStream>(into target: inout Target) {
    target.write("\(type) ")
    name.writeCode(into: &target)
    if let value = initializer {
      target.write(" = ")
      value.writeCode(into: &target)
    }
    target.write(";\n")
  }

}
