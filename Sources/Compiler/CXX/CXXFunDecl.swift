/// A C++ function declaration.
public struct CXXFunDecl {

  /// The ID of a C++ function in its module.
  public typealias ID = Int

  /// A parameter in a C++ function declaration.
  public typealias Parameter = (name: String, type: CXXTypeExpr)

  /// The identifier of the function.
  public let identifier: CXXIdentifier

  /// The output type of the function.
  public let output: CXXTypeExpr

  /// The parameters of the function.
  public let parameters: [Parameter]

  /// Emits the forward declaration of the function.
  public func emitForwardDeclaration() -> String {
    signature + ";"
  }

  /// Emits the deginition of the function.
  public func emitDefinition() -> String {
    signature + " {}"
  }

  /// The signature of the function.
  private var signature: String {
    let ps = parameters.lazy.map({ (p) in p.type.description }).joined(separator: ", ")
    return "\(output) \(identifier)(\(ps))"
  }

}
