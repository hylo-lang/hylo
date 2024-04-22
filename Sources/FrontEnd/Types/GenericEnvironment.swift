import Algorithms
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

  /// The generic parameters introduced in the environment, in the order there declaration appears
  /// in Hylo sources.
  public private(set) var parameters: [GenericParameterDecl.ID]

  /// The uninstantiated type constraints.
  public private(set) var constraints: [GenericConstraint] = []

  /// A table from types to their entry.
  private var ledger: [AnyType: Int] = [:]

  /// The equivalence classes and their associated conformance sets.
  private var entries: [EquivalenceClass] = []

  /// Creates an environment that introduces `parameters` in a declaration space.
  public init(of id: AnyDeclID, introducing parameters: [GenericParameterDecl.ID]) {
    self.decl = id
    self.parameters = parameters
  }

  /// `true` if this environment doesn't introduce any generic parameter or constraint.
  public var isEmpty: Bool {
    parameters.isEmpty && constraints.isEmpty
  }

  /// Returns the set of traits to which `type` conforms in t`self`.
  public func conformedTraits(of type: AnyType) -> Set<TraitType> {
    if let i = ledger[type] {
      return entries[i].conformances
    } else {
      return []
    }
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
  }

  /// Inserts in this environment the parameters and constraints introduced by `parent`.
  ///
  /// - Parameters:
  ///   - parent A generic environment from which `self` inherits, either because `self` is
  ///     notionally contained in `parent` or because `self` and `parent` are trait environments
  ///     and `self` refines `parent`.
  mutating func extend(_ parent: Self) {
    // Fast path: `parent` is empty.
    if parent.isEmpty {
      return
    }

    // Order parameters introduced by `parent` first.
    parameters = Array(
      chain(parent.parameters, parameters.lazy.filter({ !parent.parameters.contains($0) })))

    // Merge equivalence classes.
    if ledger.isEmpty {
      ledger = parent.ledger
      entries = parent.entries
    } else {
      for (t, i) in parent.ledger {
        if let j = ledger[t] {
          entries[j].equivalences.formUnion(parent.entries[i].equivalences)
          entries[j].conformances.formUnion(parent.entries[i].conformances)
        } else {
          ledger[t] = entries.count
          entries.append(parent.entries[i])
        }
      }
    }

    // Merge user constraints.
    constraints.append(contentsOf: parent.constraints)
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
