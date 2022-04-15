/// An object that provides context to interpret generic parameters.
struct GenericEnvironment {

  /// The uninstantiated type constraints.
  public let constraints: [TypeConstraint]

  /// A table from types to their entry.
  private var ledger: [Type: Int] = [:]

  /// The equivalence classes and their associated conformance sets.
  private var entries: [(equivalences: Set<Type>, conformances: Set<TraitType>)] = []

  /// Creates the generic environment of `decl` with the specified constraints, using `checker` to
  /// evaluate them.
  init<T: DeclID>(decl: T, constraints: [TypeConstraint], into checker: inout TypeChecker) {
    self.constraints = constraints

    let scope = AnyNodeID(decl)
    for c in constraints {
      switch c {
      case .equality(let l, let r):
        registerEquivalence(l: l, r: r)

      case .conformance(let l, let traits):
        let allTraits: Set<TraitType> = traits.reduce(into: [], { ts, t in
          assert(t.base is TraitType)
          ts.formUnion(checker.conformedTraits(of: t, inScope: scope))
        })
        registerConformance(l: l, traits: allTraits)

      case .size:
        break
      }
    }
  }

  /// Returns the set of traits to which `type` conforms in the environment.
  func conformedTraits(of type: Type) -> Set<TraitType> {
    if let i = ledger[type] {
      return entries[i].conformances
    } else {
      return []
    }
  }

  private mutating func registerEquivalence(l: Type, r: Type) {
    if let i = ledger[l] {
      // `l` is part of a class.
      if let j = ledger[r] {
        // `r` is part of a class too; merge the entries.
        entries[i].equivalences.formUnion(entries[j].equivalences)
        entries[i].conformances.formUnion(entries[j].conformances)
        entries[j] = ([], [])
      } else {
        // `r` isn't part of a class.
        entries[i].equivalences.insert(r)
      }

      // Update the ledger for `r`.
      ledger[r] = i
    } else if let j = ledger[r] {
      // `l` isn't part of a class, but `r` is.
      ledger[l] = j
      entries[j].equivalences.insert(l)
    } else {
      // Neither `l` nor `r` are part of a class.
      ledger[l] = entries.count
      ledger[l] = entries.count
      entries.append((equivalences: [l, r], conformances: []))
    }
  }

  private mutating func registerConformance(l: Type, traits: Set<TraitType>) {
    if let i = ledger[l] {
      // `l` is part of a class.
      entries[i].conformances.formUnion(traits)
    } else {
      // `l` isn't part of a class.
      ledger[l] = entries.count
      entries.append((equivalences: [l], conformances: traits))
    }
  }

}
