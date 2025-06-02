/// A term denoting a type checking error.
public struct ErrorTerm: TermProtocol, Sendable {

  public var flags: ValueFlags { .hasError }

}

extension ErrorTerm: CustomStringConvertible {

  public var description: String { "_" }

}
