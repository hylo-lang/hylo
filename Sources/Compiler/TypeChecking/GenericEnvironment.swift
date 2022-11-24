import Utils

/// An object that provides context to interpret generic parameters.
struct GenericEnvironment {

  /// The uninstantiated type constraints.
  public let constraints: [Constraint]

  /// A table from types to their entry.
  private var ledger: [AnyType: Int] = [:]

  /// The equivalence classes and their associated conformance sets.
  private var entries: [(equivalences: Set<AnyType>, conformances: Set<TraitType>)] = []

  /// Creates and returns the generic environment of `decl`, or returns `nil` if one of the
  /// specified constraints are ill-formed.
  ///
  /// - Parameters:
  ///   - decl: The declaration for which `self` is being initialized.
  ///   - constraints: The constraints defined on the environment to build.
  ///   - checker: The type checker used to check the constraints.
  init?<T: DeclID>(decl: T, constraints: [Constraint], into checker: inout TypeChecker) {
    self.constraints = constraints

    let scope = AnyScopeID(decl)!
    for c in constraints {
      switch c {
      case let equality as EqualityConstraint:
        registerEquivalence(l: equality.left, r: equality.right)

      case let conformance as ConformanceConstraint:
        var allTraits: Set<TraitType> = []
        for trait in conformance.traits {
          guard let bases = checker.conformedTraits(of: AnyType(trait), inScope: scope)
          else { return nil }
          allTraits.formUnion(bases)
        }
        registerConformance(l: conformance.subject, traits: allTraits)

      case is PredicateConstraint:
        break

      default:
        unreachable()
      }
    }
  }

  /// Returns the set of traits to which `type` conforms in the environment.
  func conformedTraits(of type: AnyType) -> Set<TraitType> {
    if let i = ledger[type] {
      return entries[i].conformances
    } else {
      return []
    }
  }

  private mutating func registerEquivalence(l: AnyType, r: AnyType) {
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

  private mutating func registerConformance(l: AnyType, traits: Set<TraitType>) {
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
