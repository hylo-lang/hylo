/// The possibly synthetic declaration of a parameter in a callable entity's declaration.
public struct CallableParameterDecl {

  /// The declaration of the parameter, if any.
  ///
  /// This property denotes either a parameter or a capture.
  public let decl: AnyDeclID?

  /// The type of the parameter.
  public let type: ParameterType

  /// Creates an instance with given properties.
  public init(decl: AnyDeclID?, type: ParameterType) {
    self.decl = decl
    self.type = type
  }

}
