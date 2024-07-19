/// A term in a rewriting system.
protocol RewritingTerm: Equatable, Collection where Index == Int, Element: Hashable {

  /// Creates a term with the given symbols.
  init<S: Sequence<Element>>(_ s: S)

  /// Returns a copy of `self` in which occurrences of `s` have been replaced by `t`.
  func substituting(_ s: Self, for t: Self) -> Self

  /// Returns `u` concatenated with `v`.
  static func + (u: Self, v: Self) -> Self

  /// Returns `u` concatenated with `v`.
  static func + <S: Sequence<Element>>(u: Self, v: S) -> Self

  /// Returns `u` concatenated with `v`.
  static func + <S: Sequence<Element>>(u: S, v: Self) -> Self

}
