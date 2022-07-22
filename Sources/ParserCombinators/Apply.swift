/// A combinator that applies a closure.
public struct Apply<Context, Element>: ParserCombinator {

  /// The action to apply.
  public let action: (inout Context) throws -> Element?

  /// Creates a combinator applying the specified closure.
  public init(_ action: @escaping (inout Context) throws -> Element?) {
    self.action = action
  }

  public func parse(_ context: inout Context) throws -> Element? {
    try action(&context)
  }

}
