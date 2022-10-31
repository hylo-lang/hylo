/// An implicit parameter in a function or subscript declaration.
public struct ImplicitParameter: Codable {

  /// The name of the parameter.
  let name: String

  /// The declaration of the parameter.
  let decl: AnyDeclID

}
