/// A generic type parameter.
public struct GenericTypeParamType: TypeProtocol, Hashable {

  /// The declaration that introduces the parameter.
  ///
  /// - Note: The ID may denote the declaration of a generic type parameter, associated type, or
  ///   trait. In the latter case, the type denotes the trait's synthesized `Self` parameter.
  public let decl: AnyDeclID

  public let flags: TypeFlags = [.isCanonical, .hasGenericTypeParam]

  public func canonical() -> Type { .genericTypeParam(self) }

}
