import Utils

/// An object that provides context to interpret the generic parameters of a declaration.
public struct GenericEnvironment {

  /// An equivalence class and its associated conformances.
  private struct EquivalenceClass: Equatable {

    /// A set of types known to be equivalent to each others.
    var equivalences: Set<AnyType> = []

    /// The set of traits to which types in this class are known to conform.
    var conformances: Set<TraitType> = []

  }

  /// The generic parameters introduced in the environment, in the order there declaration appears
  /// in Hylo sources.
  public let parameters: [GenericParameterDecl.ID]

  /// The uninstantiated type constraints.
  public private(set) var constraints: [GenericConstraint] = []

  /// A table from types to their entry.
  private var ledger: [AnyType: Int] = [:]

  /// The equivalence classes and their associated conformance sets.
  private var entries: [EquivalenceClass] = []

  /// Creates an environment that introduces `parameters` in a declaration space.
  public init(introducing parameters: [GenericParameterDecl.ID]) {
    self.parameters = parameters
  }

  /// Returns the set of traits to which `type` conforms in the environment.
  public func conformedTraits(of type: AnyType) -> Set<TraitType> {
    if let i = ledger[type] {
      return entries[i].conformances
    } else {
      return []
    }
  }

  /// Inserts constraint `c` to the environment, updating equivalence classes.
  public mutating func insertConstraint(_ c: GenericConstraint) {
    constraints.append(c)
    switch c.value {
    case .equality(let lhs, let rhs):
      registerEquivalence(lhs, rhs)
    case .conformance(let lhs, let rhs):
      registerConformance(lhs, to: rhs)
    default:
      break
    }
  }

  private mutating func registerEquivalence(_ l: AnyType, _ r: AnyType) {
    if let i = ledger[l] {
      // `l` is part of a class.
      if let j = ledger[r] {
        // `r` is part of a class too; merge the entries.
        entries[i].equivalences.formUnion(entries[j].equivalences)
        entries[i].conformances.formUnion(entries[j].conformances)
        entries[j] = .init()
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
      entries.append(.init(equivalences: [l, r], conformances: []))
    }
  }

  private mutating func registerConformance(_ l: AnyType, to trait: TraitType) {
    if let i = ledger[l] {
      // `l` is part of a class.
      entries[i].conformances.insert(trait)
    } else {
      // `l` isn't part of a class.
      ledger[l] = entries.count
      entries.append(.init(equivalences: [l], conformances: [trait]))
    }
  }

}

extension GenericEnvironment: Equatable {

  public static func == (l: Self, r: Self) -> Bool {
    guard l.parameters == r.parameters else { return false }

    var s = Set(0 ..< r.entries.count)
    for (t, i) in l.ledger {
      guard let j = r.ledger[t] else { return false }
      guard l.entries[i] == r.entries[j] else { return false }
      s.remove(j)
    }

    return s.isEmpty
  }

}
