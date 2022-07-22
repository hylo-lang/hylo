/// A combinator that transforms the result of another one.
public struct Transform<Base: ParserCombinator, Element>: ParserCombinator {

  public typealias Context = Base.Context

  /// The underlying combinator.
  public let base: Base

  /// The closure that is applied to transform `base`'s result.
  public let transform: (inout Context, Base.Element) throws -> Element

  public func parse(_ context: inout Context) throws -> Element? {
    if let result = try base.parse(&context) {
      return try transform(&context, result)
    } else {
      return nil
    }
  }

}
