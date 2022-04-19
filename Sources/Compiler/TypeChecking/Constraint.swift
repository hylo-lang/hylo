/// A constraint.
public enum Constraint: Hashable {

  /// A constraint `L == R` specifying that `L` is exactly the same type as `R`.
  ///
  /// - Note: This constraint is commutative.
  case equality(l: Type, r: Type)

  /// A constraint `L <: R` specifying that `L` is a subtype of `R`.
  case subtyping(l: Type, r: Type)

  /// A constraint `L : T1 & ... & Tn` specifying that `L` conforms to the traits `T1, ..., Tn`.
  case conformance(l: Type, traits: Set<TraitType>)

  /// A size constraint denoting a predicate over size parameters.
  case size(AnyExprID)

  /// Calls `visitor` with mutable projections of the types contained in this constraint.
  ///
  /// - Note: The traits on the right hand side of a conformance constraint are not visited.
  public mutating func visitTypes(_ visitor: (inout Type) -> Void) {
    switch self {
    case .equality(var l, var r):
      visitor(&l)
      visitor(&r)
      self = .equality(l: l, r: r)

    case .subtyping(var l, var r):
      visitor(&l)
      visitor(&r)
      self = .subtyping(l: l, r: r)

    case .conformance(var l, let traits):
      visitor(&l)
      self = .conformance(l: l, traits: traits)

    case .size:
      return
    }
  }

  /// Returns a textual description of that constraint.
  public func describe(in ast: AST) -> String {
    switch self {
    case .equality(let l, let r):
      return "\(l) == \(r)"

    case .subtyping(l: let l, r: let r):
      return "\(l) <: \(r)"

    case .conformance(let l, let traits):
      let t = traits.map({ "\($0)" }).joined(separator: ", ")
      return "\(l): \(t)"

    case .size(let e):
      return "expr"
    }
  }

}
