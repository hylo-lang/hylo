/// A combinator that attempts a parse and backtracks if that produced a hard failure.
public struct Attempt<Base: ParserCombinator>: ParserCombinator
where Base.Context: Restorable
{

  public typealias Context = Base.Context

  public typealias Element = Base.Element?

  /// The underlying combinator.
  public let base: Base

  /// Creates an `Attempt` combinator that wraps `base`.
  public init(_ base: Base) {
    self.base = base
  }

  public func parse(_ context: inout Context) throws -> Element? {
    let backup = context.backup()
    do {
      return .some(try base.parse(&context))
    } catch {
      context.restore(from: backup)
      return .some(nil)
    }
  }

  /// Creates a combinator that applies `self` and then `other` without committing unless either
  /// `self` or `other` did.
  public func andCollapsingSoftFailures<Other: ParserCombinator>(
    _ other: Other
  ) -> Apply<Base.Context, (Element, Other.Element)> where Other.Context == Context {
    Apply({ (context) in
      if let a = try self.parse(&context) {
        if let b = try other.parse(&context) {
          return (a, b)
        } else if a == nil {
          return nil
        } else {
          throw HardFailure()
        }
      } else {
        return nil
      }
    })

  }

}
