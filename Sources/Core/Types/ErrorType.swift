/// A type denoting a type checking error.
public struct ErrorType: TypeProtocol {

  public var flags: TypeFlags { [.isCanonical, .hasError] }

}

extension ErrorType: CustomStringConvertible {

  public var description: String { "%error%" }

}
