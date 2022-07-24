/// A combinator that as many times as possible another combinator.
public struct ZeroOrMany<Base: ParserCombinator>: ParserCombinator {

  public typealias Context = Base.Context

  public typealias Element = [Base.Element]

  /// The underlying combinator.
  public let base: Base

  /// Creates a `ZeroOrMany` combinator that wraps `base`.
  public init(_ base: Base) {
    self.base = base
  }

  public func parse(_ context: inout Context) throws -> Element? {
    var elements: [Base.Element] = []
    while let next = try base.parse(&context) {
      elements.append(next)
    }
    return elements
  }

}
