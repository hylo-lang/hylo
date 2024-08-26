/// A term denoting a type checking error.
public struct ErrorTerm: TermProtocol {

  public var flags: ValueFlags { .hasError }

}

extension ErrorTerm: CustomStringConvertible {

  public var description: String { "_" }

}
