/// A combinator that attempts a parse with multiple combinators until one succeeds.
public struct ChooseN<Base: ParserCombinator>: ParserCombinator {

  public typealias Context = Base.Context

  public typealias Element = Base.Element

  /// The underlying combinators.
  public let bases: [Base]

  /// Creates a combinator that applies `bases` sequentially until one returns an element or
  /// returns a hard failure.
  public init<S: Sequence>(_ bases: S) where S.Element == Base {
    self.bases = Array(bases)
    precondition(!self.bases.isEmpty)
  }

  public func parse(_ context: inout Context) throws -> Element? {
    for base in bases {
      if let result = try base.parse(&context) {
        return result
      }
    }
    return nil
  }

}
