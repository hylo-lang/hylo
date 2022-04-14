/// A type denoting a type checking error.
public struct ErrorType: TypeProtocol, Hashable {

  public let flags: TypeFlags = [.isCanonical, .hasError]

}

extension ErrorType: CustomStringConvertible {

  public var description: String { "%error%" }

}
