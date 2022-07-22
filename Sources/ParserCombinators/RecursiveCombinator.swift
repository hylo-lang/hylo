/// A combinator that can be used recursively.
public struct RecursiveCombinator<Context, Element>: ParserCombinator {

  private final class Definition {

    var parse: ((inout Context) throws -> Element?)?

    init() {}

  }

  /// The definition of the combinator.
  private let definition = Definition()

  /// Declares a recursive combinator whose `parse(_:)` method must be defined separately.
  public init() {}

  /// Indiates whether `self` has been defined.
  public var isDefined: Bool { definition.parse != nil }

  /// Defines the combinator if it wasn't already.
  public func define<T: ParserCombinator>(_ combinator: T)
  where T.Context == Context, T.Element == Element
  {
    precondition(definition.parse == nil)
    definition.parse = combinator.parse(_:)
  }

  /// Defines the combinator.
  public func define(_ parse: @escaping (inout Context) -> Element?) {
    precondition(definition.parse == nil)
    definition.parse = parse
  }

  public func parse(_ context: inout Context) throws -> Element? {
    try definition.parse!(&context)
  }

}
