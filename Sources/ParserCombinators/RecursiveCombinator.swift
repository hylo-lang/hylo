/// A combinator that can be used recursively.
public struct RecursiveCombinator<Context, Element>: ParserCombinator {

  private final class Definition {

    var make: () -> (inout Context) throws -> Element?

    var parse: ((inout Context) throws -> Element?)?

    init(make: @escaping () -> (inout Context) throws -> Element?) {
      self.make = make
    }

  }

  /// The definition of the combinator.
  private let definition: Definition

  /// Declares combinator that forwards operation to combinator returned by `makeParse`.
  public init(_ makeParse: @autoclosure @escaping () -> (inout Context) throws -> Element?) {
    self.definition = Definition(make: makeParse)
  }

  public func parse(_ context: inout Context) throws -> Element? {
    if definition.parse == nil {
      definition.parse = definition.make()
    }
    return try definition.parse!(&context)
  }

}
