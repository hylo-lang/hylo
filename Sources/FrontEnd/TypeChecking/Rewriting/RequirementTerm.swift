import Utils

/// A set of rewriting rules describing the requirements of a generic signature.
typealias RequirementSystem = RewritingSystem<RequirementTerm>

/// A rule in a requirement rewriting system.
typealias RequirementRule = RewritingRule<RequirementTerm>

/// A term corresponding to a type or trait in a generic requirement.
struct RequirementTerm: RewritingTerm {

  /// The parts of the term.
  let symbols: [RequirementSymbol]

  /// Creates a term with the given symbols.
  init<S: Sequence<RequirementSymbol>>(_ s: S) {
    self.symbols = Array(s)
  }

  /// The generic parameter at the root of this term, if any.
  var rootParameter: GenericParameterDecl.ID? {
    symbols.first.flatMap { (s) in
      if case .parameterType(let p) = s {
        return p
      } else {
        return nil
      }
    }
  }

  /// Returns `self` suffixed by `s`.
  func appending(_ s: RequirementSymbol) -> Self {
    .init(symbols + [s])
  }

  /// Returns a copy of `self` in which occurrences of `s` have been replaced by `t`.
  func substituting(_ s: RequirementTerm, for t: RequirementTerm) -> Self {
    .init(symbols.replacing(s.symbols, with: t.symbols))
  }

  /// Returns `u` concatenated with `v`.
  static func + (u: Self, v: Self) -> Self {
    .init(u.symbols + v.symbols)
  }

  /// Returns `u` concatenated with `v`.
  static func + <S: Sequence<RequirementSymbol>>(u: Self, v: S) -> Self {
    .init(u.symbols + v)
  }

  /// Returns `u` concatenated with `v`.
  static func + <S: Sequence<RequirementSymbol>>(u: S, v: Self) -> Self {
    .init(u + v.symbols)
  }

}

extension RequirementTerm: ExpressibleByArrayLiteral {

  init(arrayLiteral elements: RequirementSymbol...) {
    self.init(elements)
  }

}

extension RequirementTerm: Collection {

  typealias Element = RequirementSymbol

  typealias Index = Int

  var startIndex: Int { 0 }

  var endIndex: Int { symbols.count }

  func index(after p: Int) -> Int { p + 1 }

  subscript(p: Int) -> RequirementSymbol { symbols[p] }

}

/// A part of a term in a requirement rewriting system.
enum RequirementSymbol: Hashable {

  /// An associated type.
  case associatedType(AssociatedTypeDecl.ID)

  /// A generic parameter type.
  case parameterType(GenericParameterDecl.ID)

  /// A trait.
  case trait(TraitDecl.ID)

  /// A concrete type constraint.
  case concrete(AnyType)

  /// The kind of this symbol.
  var kind: Int {
    switch self {
    case .associatedType:
      return 0
    case .parameterType:
      return 1
    case .trait:
      return 2
    case .concrete:
      return 3
    }
  }

}
