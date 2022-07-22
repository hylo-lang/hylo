/// A combinator that attempts a parse with a combinator and falls back to another one after
/// backtracking when the former produces any kind of failure.
public struct TryCatch<First: ParserCombinator, Second: ParserCombinator>: ParserCombinator
where First.Context: Restorable,
      First.Context == Second.Context,
      First.Element == Second.Element
{

  public typealias Context = First.Context

  public typealias Element = First.Element

  /// The first combinator.
  public let first: First

  /// The second combinator.
  public let second: Second

  /// Creates a combinator that applies `first`, or backtracks and applies `second` if `first`
  /// produced any kind of failure.
  public init(trying first: First, orCatchingAndApplying second: Second) {
    self.first = first
    self.second = second
  }

  public func parse(_ context: inout Context) throws -> Element? {
    let backup = context.backup()
    do {
      if let element = try first.parse(&context) { return element }
    } catch {}
    context.restore(from: backup)
    return try second.parse(&context)
  }

}
