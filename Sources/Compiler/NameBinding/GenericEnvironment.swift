import Utils

struct GenericEnvironment {

  /// The generic clause type parameters introduced in this environment.
  let params: [GenericParamDecl]

  /// A table from type parameter declaration to their entry.
  var ledger: [ObjectIdentifier: Int]

  /// The equivalence classes and their associated conformance sets.
  var entries: [(equivalences: [IdentSign], conformances: Set<ViewTypeDecl>)]

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

  func conformanceSet(of param: GenericParamDecl) -> Set<ViewTypeDecl>? {
    guard let i = ledger[ObjectIdentifier(param)] else { return nil }
    return entries[i].conformances
  }

  private mutating func insert(equivalenceBetween lhs: Sign, and rhs: Sign) {
    if let lid = lhs as? BareIdentSign,
       let rid = rhs as? IdentSign,
       let lpar = params.first(where: { $0.name == lid.name }),
       lpar.name != rid.components[0].name
    {
      // Check if RHS is a sibling.
      let rpar = (rid as? BareIdentSign).flatMap({ ri in
        params.first(where: { $0.name == ri.name })
      })

      // The LHS is a type parameter introduced by the clause and the RHS is a type identifier.
      if let i = ledger[ObjectIdentifier(lpar)] {
        // LHS is already part of a class.
        if let rpar = rpar {
          // RHS is a sibling.
          if let j = ledger[ObjectIdentifier(rpar)] {
            // RHS is already part of a class too. Merge the entries.
            for ident in entries[j].equivalences
            where !entries[i].equivalences.contains(where: { $0 === ident })
            {
              entries[i].equivalences.append(ident)
            }
            entries[i].conformances.formUnion(entries[j].conformances)
            entries[j] = ([], [])
          } else if !entries[i].equivalences.contains(where: { $0 === rid }) {
            // RHS is a sibling, but isn't part of a class yet.
            entries[i].equivalences.append(rid)
          }

          // Insert/update the entry for RHS.
          ledger[ObjectIdentifier(rpar)] = i
        } else if !entries[i].equivalences.contains(where: { $0 === rid }) {
          // RHS is not a sibling.
          entries[i].equivalences.append(rid)
        }
      } else if let rpar = rpar {
        // LHS isn't part of a class, but RHS is a sibling.
        if let j = ledger[ObjectIdentifier(rpar)] {
          // RHS is already part of a class.
          if !entries[j].equivalences.contains(where: { $0 === lid }) {
            entries[j].equivalences.append(lid)
            ledger[ObjectIdentifier(lpar)] = j
          }
        } else {
          // Neither LHS or RHS are part of a class yet.
          ledger[ObjectIdentifier(lpar)] = entries.count
          ledger[ObjectIdentifier(rpar)] = entries.count
          entries.append((equivalences: [lid, rid], conformances: []))
        }
      } else {
        // LHS isn't part of a class and RHS isn't a sibling.
        ledger[ObjectIdentifier(lpar)] = entries.count
        entries.append((equivalences: [lid, rid], conformances: []))
      }
    } else if rhs is BareIdentSign {
      insert(equivalenceBetween: rhs, and: lhs)
    }
  }

  private mutating func insert(
    conformanceOf lhs: Sign,
    to rhs: Sign,
    space: DeclSpace,
    binder: inout NameBinder
  ) {
    // LHS must be a type parameter and RHS must be a type identifier.
    guard let lid = lhs as? BareIdentSign,
          let rid = rhs as? IdentSign,
          let lpar = params.first(where: { $0.name == lid.name })
    else { return }

    // Identifiers rooted at type parameters cannot resolve to a view. Bail out here to avoid
    // infinite recursion through lookup into the environment.
    guard !params.contains(where: { $0.name == rid.components[0].name }) else { return }

    // Resolve RHS as a view declaration.
    if let v = binder.resolve(rid, unqualifiedFrom: space) as? ViewTypeDecl {
      if let i = ledger[ObjectIdentifier(lpar)] {
        // LHS is already part of a class.
        modify(value: &entries[i].conformances, with: { s in
          if s.insert(v).inserted {
            s.formUnion(binder.conformanceSet(of: v))
          }
        })
      } else {
        // LHS isn't part of a class yet.
        ledger[ObjectIdentifier(lpar)] = entries.count
        var s = binder.conformanceSet(of: v)
        s.insert(v)
        entries.append((equivalences: [lid], conformances: s))
      }
    }
  }

}
