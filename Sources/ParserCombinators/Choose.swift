/// A combinator that attempts a parse with a combinator and falls back to another one when the
/// former returns soft failures.
public struct Choose<First: ParserCombinator, Second: ParserCombinator>: ParserCombinator
where First.Context == Second.Context, First.Element == Second.Element
{

  public typealias Context = First.Context

  public typealias Element = First.Element

  /// The first combinator.
  public let firstCombinator: First

  /// The second combinator.
  public let secondCombinator: Second

  /// Creates a combinator that applies `first`, or backtracks and applies `second` if `first`
  /// returned a soft failure.
  public init(_ first: First, or second: Second) {
    self.firstCombinator = first
    self.secondCombinator = second
  }

  public func parse(_ context: inout Context) throws -> Element? {
    if let result = try firstCombinator.parse(&context) {
      return result
    } else {
      return try secondCombinator.parse(&context)
    }
  }

}
