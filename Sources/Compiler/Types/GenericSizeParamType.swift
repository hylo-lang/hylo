/// A generic size parameter.
public struct GenericSizeParamType: TypeProtocol, Hashable {

  /// The declaration that introduces the parameter.
  ///
  /// - Note: The index may denote the declaration of a generic type parameter or associated size.
  public let decl: AnyDeclIndex

  public let flags: TypeFlags = [.isCanonical, .hasGenericSizeParam]

  public func canonical() -> Type { .genericSizeParam(self) }

}
