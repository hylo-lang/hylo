import Core

/// A C++ class declaration.
public struct CXXClassDecl {

  /// The ID of a C++ class in its module.
  public typealias ID = Int

  /// The name of the function.
  public let name: CXXIdentifier

  /// The original node in Val AST.
  let original: ProductTypeDecl.Typed

  /// Writes the signature of the class into `target`.
  public func writeSignature<Target: TextOutputStream>(into target: inout Target) {
    target.write("class \(name)")
  }

  /// Writes the definition of the class into `target`.
  public func writeDefinition<Target: TextOutputStream>(into target: inout Target) {
    target.write("{\n")
    target.write("};\n")
  }

}
