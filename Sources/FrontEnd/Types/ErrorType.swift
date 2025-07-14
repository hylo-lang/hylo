/// A type denoting a type checking error.
public struct ErrorType: TypeProtocol, Sendable {

  public var flags: ValueFlags { .hasError }

}

extension ErrorType: CustomStringConvertible {

  public var description: String { "_" }

}
