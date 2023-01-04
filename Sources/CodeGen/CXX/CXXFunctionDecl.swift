/// A C++ function declaration.
public struct CXXFunctionDecl {

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

  /// Writes the signature of the function into `target`.
  public func writeSignature<Target: TextOutputStream>(into target: inout Target) {
    target.write("\(output) \(identifier)(")
    for i in 0 ..< parameters.count {
      if i != 0 { target.write(", ") }
      target.write("\(parameters[i].type) \(parameters[i].name)")
    }
    target.write(")")
  }

}
