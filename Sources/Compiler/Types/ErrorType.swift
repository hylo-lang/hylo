/// A type denoting a type checking error.
public struct ErrorType: TypeProtocol, Hashable {

  public let flags: TypeFlags = [.isCanonical, .hasError]

  public func canonical() -> Type { .error(self) }

}
