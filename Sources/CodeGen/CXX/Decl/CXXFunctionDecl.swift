import Core

/// A C++ function declaration.
public struct CXXFunctionDecl: CXXTopLevelDecl {

  /// The ID of a C++ function in its module.
  public typealias ID = Int

  /// A parameter in a C++ function declaration.
  public typealias Parameter = (name: CXXIdentifier, type: CXXTypeExpr)

  /// The identifier of the function.
  public let identifier: CXXIdentifier

  /// The output type of the function.
  public let output: CXXTypeExpr

  /// The parameters of the function.
  public let parameters: [Parameter]

  /// The body of the function.
  public let body: CXXRepresentable?

  /// The original node in Val AST.
  let original: FunctionDecl.Typed

  /// Writes the signature of the function into `target`.
  public func writeSignature<Target: TextOutputStream>(into target: inout Target) {
    target.write("\(output) \(identifier)(")
    for i in 0 ..< parameters.count {
      if i != 0 { target.write(", ") }
      target.write("\(parameters[i].type) \(parameters[i].name)")
    }
    target.write(")")
  }

  public func writeDeclaration<Target: TextOutputStream>(into target: inout Target) {
    writeSignature(into: &target)
    target.write(";\n")
  }
  public func writeDefinition<Target: TextOutputStream>(into target: inout Target) {
    writeSignature(into: &target)
    if body != nil {
      target.write(" ")
      body!.writeCode(into: &target)
    } else {
      target.write(";\n")
    }
  }
}
