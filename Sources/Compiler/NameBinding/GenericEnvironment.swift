import Utils

struct GenericEnvironment {

  typealias Equivalences = Set<Key>

  struct Key: Hashable {

    var components: [String]

    init<S>(_ components: S) where S: Sequence, S.Element == String {
      self.components = Array(components)
    }

    init(_ ident: IdentSign) {
      self.components = ident.components.map({ $0.name })
    }

  }

  /// The generic clause type parameters introduced in this environment.
  let params: [GenericParamDecl]

  /// A table from type identifiers to their entry.
  var ledger: [Key: Int]

  /// The equivalence classes and their associated conformance sets.
  var entries: [(equivalences: Equivalences, conformances: Set<ViewTypeDecl>)]

  /// Builds an environment from the generic generic clause of a declaration.
  init(decl: BaseGenericDecl, binder: inout NameBinder) {
    self.ledger = [:]
    self.entries = []

    // Nothing to do if the `decl` has no generic clause.
    guard let clause = decl.genericClause else {
      self.params = []
      return
    }
    self.params = clause.params

    for req in clause.typeReqs {
      switch req.kind {
      case .equality:
        insert(equivalenceBetween: req.lhs, and: req.rhs)
      case .conformance:
        insert(conformanceOf: req.lhs, to: req.rhs, space: decl.parentDeclSpace!, binder: &binder)
      }
    }
  }

  /// Returns the set of views defined for `key`.
  func conformanceSet(of key: Key) -> Set<ViewTypeDecl>? {
    guard let i = ledger[key] else { return nil }
    return entries[i].conformances
  }

  private mutating func insert(equivalenceBetween lhs: Sign, and rhs: Sign) {
    // Nothing to do if the requirement isn't defined on identifiers.
    guard let lid = lhs as? IdentSign,
          let rid = rhs as? IdentSign
    else { return }

    let lbox = Key(lid)
    let rbox = Key(rid)

    if let i = ledger[lbox] {
      // LHS is part of a class.
      if let j = ledger[rbox] {
        // RHS is already part of a class too; merge the entries.
        entries[i].equivalences.formUnion(entries[j].equivalences)
        entries[i].conformances.formUnion(entries[j].conformances)
        entries[j] = ([], [])
      } else {
        // RHS isn't part of a class.
        entries[i].equivalences.insert(rbox)
      }

      // Update the ledger for RHS.
      ledger[rbox] = i
    } else if let j = ledger[rbox] {
      // LHS isn't part of a class, but RHS is.
      ledger[lbox] = j
      entries[j].equivalences.insert(lbox)
    } else {
      // Neither LHS nor RHS are part of a class; create a new entry.
      ledger[lbox] = entries.count
      ledger[rbox] = entries.count
      entries.append((equivalences: [lbox, rbox], conformances: []))
    }
  }

  private mutating func insert(
    conformanceOf lhs: Sign,
    to rhs: Sign,
    space: DeclSpace,
    binder: inout NameBinder
  ) {
    // Nothing to do if the requirement isn't defined on identifiers.
    guard let lid = lhs as? IdentSign,
          let rid = rhs as? IdentSign
    else { return }

    let lbox = Key(lid)

    // Resolve RHS as a view declaration.
    if let v = binder.resolve(rid, unqualifiedFrom: space) as? ViewTypeDecl {
      if let i = ledger[lbox] {
        // LHS is part of a class.
        modify(value: &entries[i].conformances, with: { s in
          if s.insert(v).inserted {
            s.formUnion(binder.conformanceSet(of: v))
          }
        })
      } else {
        // LHS isn't part of a class.
        ledger[lbox] = entries.count
        var s = binder.conformanceSet(of: v)
        s.insert(v)
        entries.append((equivalences: [lbox], conformances: s))
      }
    }
  }

}
