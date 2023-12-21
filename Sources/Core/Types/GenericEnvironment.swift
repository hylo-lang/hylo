import Utils

/// Context to interpret the generic parameters of a declaration.
public struct GenericEnvironment {

  /// An equivalence class and its associated conformances.
  private struct EquivalenceClass: Equatable {

    /// A set of types known to be equivalent to each others.
    var equivalences: Set<AnyType> = []

    /// The set of traits to which types in this class are known to conform.
    var conformances: Set<TraitType> = []

  }

  /// The declaration associated with the environment.
  public let decl: AnyDeclID

  /// A number reflecting the number of times the environment has been updated.
  ///
  /// A generic environments is built incrementally by inserting constraints on generic parameters.
  /// Each update increments the generation number until all constraints have been inserted, at
  /// which point the environment must be finalized, setting its generation to `UInt.max`.
  public private(set) var generation: UInt = 0

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
  public init(
    of id: AnyDeclID,
    introducing parameters: [GenericParameterDecl.ID]
  ) {
    self.decl = id
    self.parameters = parameters
  }

  /// `true` iff `self` has been fully constructed and type checked.
  public var isFinalized: Bool {
    generation == .max
  }

  /// Returns the set of traits to which `type` conforms in t`self`.
  public func conformedTraits(of type: AnyType) -> Set<TraitType> {
    if let i = ledger[type] {
      return entries[i].conformances
    } else {
      return []
    }
  }

  /// Marks that `self` has been fully constructed.
  ///
  /// - Requires: `self` has not been finalized yet.
  public mutating func finalize() {
    precondition(!isFinalized)
    generation = .max
  }

  /// Inserts constraint `c` to the environment, updating equivalence classes.
  public mutating func insertConstraint(_ c: GenericConstraint) {
    // TODO: Eliminate duplicates
    constraints.append(c)
    switch c.value {
    case .equality(let lhs, let rhs):
      establishEquivalence(lhs, rhs)
    case .conformance(let lhs, let rhs):
      establishConformance(lhs, to: rhs)
    default:
      break
    }
    generation += 1
  }

  /// Registers the fact that `l` is equivalent to `r` in the context of this environment.
  public mutating func establishEquivalence(_ l: AnyType, _ r: AnyType) {
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
    generation += 1
  }

  /// Registers the fact that `l` conforms to `r` in the context of this environment.
  public mutating func establishConformance(_ l: AnyType, to r: TraitType) {
    if let i = ledger[l] {
      // `l` is part of a class.
      entries[i].conformances.insert(r)
    } else {
      // `l` isn't part of a class.
      ledger[l] = entries.count
      entries.append(.init(equivalences: [l], conformances: [r]))
    }
    generation += 1
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
