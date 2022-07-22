/// A parser combinator.
public protocol ParserCombinator {

  /// The context from which element are being parsed, typically a stream of tokens.
  ///
  /// - Requires: `Context` must have value semantics.
  associatedtype Context

  /// The element parsed by the combinator.
  associatedtype Element

  /// Attempts to parse a result from `context`.
  func parse(_ context: inout Context) throws -> Element?

}

postfix operator +

postfix operator *

extension ParserCombinator {

  /// Creates a combinator that applies `self` and then `other`.
  public func and<Other: ParserCombinator>(_ other: Other) -> Combine<Self, Other> {
    Combine(self, and: other)
  }

  /// Creates a combinator that applies `self`, or backtracks and applies `other` if `self`
  /// returned a soft failure.
  public func or<Other: ParserCombinator>(_ other: Other) -> Choose<Self, Other> {
    Choose(self, or: other)
  }

  /// Creates a combinator that applies `self`, or backtracks and applies `other` if `self`
  /// returned any kind of failure.
  public func orCatch<Other: ParserCombinator>(
    andApply other: Other
  ) -> TryCatch<Self, Other> {
    TryCatch(trying: self, orCatchingAndApplying: other)
  }

  /// Creates a combinators that transforms the result of `self`.
  public func map<T>(
    _ transform: @escaping (inout Context, Element) throws -> T
  ) -> Transform<Self, T> {
    Transform(base: self, transform: transform)
  }

  public static postfix func + (base: Self) -> OneOrMany<Self> {
    OneOrMany(base)
  }

  public static postfix func * (base: Self) -> ZeroOrMany<Self> {
    ZeroOrMany(base)
  }

}

/// Creates an `Attempt` combinator that wraps `base`.
public func maybe<Base: ParserCombinator>(_ base: Base) -> Attempt<Base> {
  Attempt(base)
}

/// Creates a `ChooseN` combinator that wraps `bases`.
public func oneOf<S: Sequence, Base: ParserCombinator>(_ bases: S) -> ChooseN<Base>
where S.Element == Base
{
  ChooseN(bases)
}

/// Creates a `OneOrMany` combinator that wraps `base`.
public func oneOrMany<Base: ParserCombinator>(_ base: Base) -> OneOrMany<Base> {
  OneOrMany(base)
}

/// Creates a `ZeroOrMany` combinator that wraps `base`.
public func zeroOrMany<Base: ParserCombinator>(_ base: Base) -> ZeroOrMany<Base> {
  ZeroOrMany(base)
}
