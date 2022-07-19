import Utils

/// A constraint.
public enum Constraint: Hashable {

  /// A set of constraints in a disjunction.
  public struct Minterm: Hashable {

    /// The constraints.
    public var constraints: [Constraint]

    /// The penalties associated with this set.
    public var penalties: Int

  }

  /// A candidate in an overload constraint.
  public struct OverloadCandidate: Hashable {

    /// The declaration of the candidate.
    public var decl: AnyDeclID

    /// The contextualized type of the declaration.
    public var type: Type

    /// The set of constraints associated with the candidate.
    public var constraints: [Constraint]

  }

  /// A constraint `L == R` specifying that `L` is exactly the same type as `R`.
  ///
  /// - Note: This constraint is commutative.
  case equality(l: Type, r: Type)

  /// A constraint `L <: R` specifying that `L` is a subtype of `R`.
  case subtyping(l: Type, r: Type)

  /// A constraint `L : T1 & ... & Tn` specifying that `L` conforms to the traits `T1, ..., Tn`.
  case conformance(l: Type, traits: Set<TraitType>)

  /// A constraint `L ⤷ R` specifying that `R` is a parameter type and `L` the type of a compatible
  /// argument.
  ///
  /// - Note: Solving a constraint `l ⤷ R` where `R` is a type variable requires that there be
  ///   another constraint on `R` fixing its parameter passing convention.
  case parameter(l: Type, r: Type)

  /// A constraint `L.m == R` specifying that `L` has a non-static member of type `R` named `m`.
  case member(l: Type, m: Name, r: Type)

  /// A value constraint denoting a predicate over value parameters.
  case value(AnyExprID)

  /// A disjunction of two or more constraint sets.
  ///
  /// Each set is associated with a penalty to represent the preferred alternatives.
  case disjunction([Minterm])

  /// A constraint specifying that a name expression refers to one of several declarations,
  /// depending on its type.
  case overload(n: NodeID<NameExpr>, l: Type, candidates: [OverloadCandidate])

  /// Creates a subtyping or equality constraint.
  public static func equalityOrSubtyping(l: Type, r: Type) -> Constraint {
    .disjunction([
      Constraint.Minterm(constraints: [.equality(l: l, r: r)], penalties: 0),
      Constraint.Minterm(constraints: [.subtyping(l: l, r: r)], penalties: 1),
    ])
  }

  /// Returns whether the constraint depends on the specified variable.
  public func depends(on variable: TypeVariable) -> Bool {
    let v = Type.variable(variable)

    switch self {
    case .equality(let l, let r):
      return (v == l) || (v == r)
    case .subtyping(let l, let r):
      return (v == l) || (v == r)
    case .conformance(let l, _):
      return (v == l)
    case .parameter(let l, let r):
      return (v == l) || (v == r)
    case .member(let l, _, let r):
      return (v == l) || (v == r)
    case .value:
      return false
    case .disjunction(let minterms):
      return minterms.contains(where: { m in
        m.constraints.contains(where: { c in c.depends(on: variable) })
      })
    case .overload(_, let l, _):
      return v == l
    }
  }

  /// Calls `modify` with a projection of the types in the constraint.
  ///
  /// - Parameters:
  ///   - modify: A closure that accepts mutable projections of the types contained in the
  ///     constraint and returns whether visitation should continue. The traits on the right hand
  ///     side of a conformance constraint are not visited.
  /// - Returns: `false` if any call to `modify` returns `false`; otherwise, `true`.
  @discardableResult
  public mutating func modifyTypes(_ modify: (inout Type) -> Bool) -> Bool {
    switch self {
    case .equality(var l, var r):
      defer { self = .equality(l: l, r: r) }
      return modify(&l) && modify(&r)

    case .subtyping(var l, var r):
      defer { self = .subtyping(l: l, r: r) }
      return modify(&l) && modify(&r)

    case .conformance(var l, let traits):
      defer { self = .conformance(l: l, traits: traits) }
      return modify(&l)

    case .parameter(var l, var r):
      defer { self = .parameter(l: l, r: r) }
      return modify(&l) && modify(&r)

    case .member(var l, let m, var r):
      defer { self = .member(l: l, m: m, r: r) }
      return modify(&l) && modify(&r)

    case .value:
      return true

    case .disjunction(var minterms):
      defer { self = .disjunction(minterms) }
      for i in 0 ..< minterms.count {
        for j in 0 ..< minterms[i].constraints.count {
          if !minterms[i].constraints[j].modifyTypes(modify) { return false }
        }
      }
      return true

    case .overload(let n, var l, var candidates):
      defer { self = .overload(n: n, l: l, candidates: candidates) }
      if !modify(&l) { return false }
      for i in 0 ..< candidates.count {
        for j in 0 ..< candidates[i].constraints.count {
          if !modify(&candidates[i].type) { return false }
          if !candidates[i].constraints[j].modifyTypes(modify) { return false }
        }
      }
      return true
    }
  }

  /// Returns whether this constraint depends on the specified type variable.

}

extension Constraint: CustomStringConvertible {

  public var description: String {
    switch self {
    case .equality(let l, let r):
      return "\(l) == \(r)"

    case .subtyping(let l, let r):
      return "\(l) <: \(r)"

    case .conformance(let l, let traits):
      let t = traits.descriptions(joinedBy: ", ")
      return "\(l) : \(t)"

    case .parameter(let l, let r):
      return "\(l) ⤷ \(r)"

    case .member(let l, let m, let r):
      return "\(l).\(m) == \(r)"

    case .value:
      return "expr"

    case .disjunction(let sets):
      return (
        sets.lazy.map { t in
          "{\(t.constraints.descriptions(joinedBy: " ∧ "))}:\(t.penalties)"
        }
      ).descriptions(joinedBy: " ∨ ")

    case .overload(let n, let l, let candidates):
      let d = candidates.lazy.map { c in
        "(\(c.decl.kind)[\(c.decl.rawValue)]+"
        + "{\(c.constraints.descriptions(joinedBy: " ∧ "  ))}"
      }

      return "NameExpr[\(n.rawValue)]:\(l) ∈ {\(d.descriptions())}"
    }
  }

}
