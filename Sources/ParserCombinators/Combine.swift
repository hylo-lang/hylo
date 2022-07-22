/// A combinator that combines the result of other combinators.
public struct Combine<First: ParserCombinator, Second: ParserCombinator>: ParserCombinator
where First.Context == Second.Context
{

  public typealias Context = First.Context

  public typealias Element = (First.Element, Second.Element)

  /// The first combinator.
  public let firstCombinator: First

  /// The second combinator.
  public let secondCombinator: Second

  /// Creates a combinator that applies `first` and then `second`.
  public init(_ first: First, and second: Second) {
    self.firstCombinator = first
    self.secondCombinator = second
  }

  public func parse(_ context: inout Context) throws -> Element? {
    if let a = try firstCombinator.parse(&context) {
      if let b = try secondCombinator.parse(&context) {
        return (a, b)
      } else {
        throw HardFailure()
      }
    } else {
      return nil
    }
  }

  /// Returns a combinator that discards the second result of `self`.
  public var first: Apply<First.Context, First.Element> {
    Apply({ (context) in (try parse(&context))?.0 })
  }

  /// Returns a combinator that discards the first result of `self`.
  public var second: Apply<First.Context, Second.Element> {
    Apply({ (context) in (try parse(&context))?.1 })
  }

}
