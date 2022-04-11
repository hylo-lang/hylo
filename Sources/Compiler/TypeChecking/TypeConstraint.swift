/// A type constraint.
public enum TypeConstraint: Hashable {

  /// A constraint `L == R` specifying that `L` is exactly the same type as `R`.
  ///
  /// - Note: This constraint is commutative.
  case equality(l: Type, r: Type)

  /// A constraint `L : T1 & ... & Tn` specifying that `L` conforms to the traits `T1, ..., Tn`.
  case conformance(l: Type, traits: Set<Type>)

  /// A size constraint denoting a predicate over size parameters.
  case size(AnyExprIndex)

  /// Calls `visitor` with mutable projections of the types contained in this constraint.
  public mutating func visitTypes(_ visitor: (inout Type) -> Void) {
    switch self {
    case var .equality(l, r):
      visitor(&l)
      visitor(&r)
      self = .equality(l: l, r: r)

    case var .conformance(l, traits):
      visitor(&l)
      traits = Set(traits.map({ var t = $0; visitor(&t); return t }))
      self = .conformance(l: l, traits: traits)

    case .size:
      return
    }
  }

  /// Returns the canonical form of this constraint.
  public func canonical() -> TypeConstraint {
    var result = self
    result.visitTypes({ $0 = $0.canonical() })
    return result
  }

}
