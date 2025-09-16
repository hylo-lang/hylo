/// A type representing the possible values of an object in an abstract interpreter.
///
/// The values of an abstract domain must form a meet-semilattice whose meet operation represents
/// the conservative superposition of two abstract values.
protocol AbstractDomain: Equatable, Sendable {

  /// Returns `lhs` merged with `rhs`.
  static func && (l: Self, r: Self) -> Self

}
