import AST
import Basic

/// A VIL pass that computes definite assignment analysis.
public struct DefiniteAssignment {

  public init() {}

  public func run(on funName: VILName, with builder: inout Builder, in context: Context) -> Bool {
    let fun = builder.module.functions[funName] ?< fatalError("function does not exist")
    let dominatorTree = DominatorTree(fun: fun)

    // Iterate over the instructions of the functions, looking for memory allocations. Note that
    // the iterator takes a copy of the function, meaning that it won't be invalidated by removals
    // and that we won't iterate over new insertions.
    var it = fun.makeInstEnumerator()
    while let (_, inst) = it.next() {
      guard let alloc = inst as? AllocStackInst else { continue }

      // First, we must collect the uses of the allocation and classify them.
      var designatedInitializer: Use?
      var stores: [Use] = []
      var loads: [Use] = []

      for use in alloc.uses {
        switch builder.module[use.userPath] {
        case is LoadInst:
          loads.append(use)

        case let user as StoreInst where user.target === alloc:
          if user.semantics == .init_ {
            assert(designatedInitializer == nil, "multiple 'init' stores to the same location")
            designatedInitializer = use
          }
          stores.append(use)

        case let user as CopyAddrInst:
          if user.target === alloc {
            if user.semantics == .init_ {
              assert(designatedInitializer == nil, "multiple 'init' stores to the same location")
              designatedInitializer = use
            }
            stores.append(use)
          } else {
            loads.append(use)
          }

        case is DeallocStackInst:
          // FIXME: Ignore stack deallocations for the time being. Later, we'll check that all uses
          // are post-dominated by one and only one deallocation.
          continue

        default:
          // The use is something else; we assume it's a load.
          loads.append(use)
        }
      }

      // If the allocation relates to a value declaration that's never used for a load, it should
      // probably be removed. DSE has the final say, but we can already emit a diagnostic.
      if loads.isEmpty {
        if let decl = alloc.decl {
          context.report(.bindingWithNoUse(decl: decl))
        }
      }

      // Next, we must determine the semantics of each store.
      if let initializer = designatedInitializer {
        // VILGen will emit a store with `init` semantics if has direct access to an initializer
        // expression (e.g., while emitting `val x = 0`). In that case, we can assume definite
        // assignment and treat all subsequent stores as modifying assignments.
        for use in stores where use != initializer {
          setSemantics(.modify, forUse: use, with: &builder)
        }
      } else if stores.count == 1 {
        // If there's only one store, that has to be an initializer.
        setSemantics(.init_, forUse: stores[0], with: &builder)
        designatedInitializer = stores[0]
      } else {
        designatedInitializer = inferStoreSemantics(
          stores: stores, dominatorTree: dominatorTree, with: &builder)
      }

      // Check that all loads are preceeded by a store. If we have a designated initializer, then
      // we must only verify that it dominates all uses. Otherwise, we must find one dominating
      // store for each use.
      if let initializer = designatedInitializer {
        for load in loads {
          if !dominatorTree.dominates(defPath: initializer.userPath, use: load) {
            context.report(.useBeforeInit(alloc: alloc, anchor: nil))
            return false
          }
        }
      } else {
        for load in loads {
          for store in stores {
            if !dominatorTree.dominates(defPath: store.userPath, use: load) {
              context.report(.useBeforeInit(alloc: alloc, anchor: nil))
              return false
            }
          }
        }
      }
    }

    return true
  }

  private func inferStoreSemantics(
    stores: [Use],
    dominatorTree: DominatorTree,
    with builder: inout Builder
  ) -> Use? {
    // Search for a designated initializer, i.e., a store that dominates all other stores.
    var designatedInitializer: Use?
    var modifiers: Set<Use> = []
    for lhs in stores where !modifiers.contains(lhs) {
      var isDominator = true
      for rhs in stores where (lhs != rhs) && !modifiers.contains(rhs) {
        if dominatorTree.dominates(defPath: lhs.userPath, use: rhs) {
          modifiers.insert(rhs)
        } else {
          isDominator = false
        }
      }

      if isDominator {
        designatedInitializer = lhs
        break
      }
    }

    // A store has `init` semantics if it dominates all other stores, or `modify` semantics if it's
    // dominated by a least one store, or `init-or-modify` semantics otherwise.
    if let initializer = designatedInitializer {
      setSemantics(.init_, forUse: initializer, with: &builder)
      for use in stores where use != initializer {
        setSemantics(.modify, forUse: use, with: &builder)
      }
    } else {
      for use in stores {
        setSemantics(
          modifiers.contains(use) ? .modify : .initOrModify,
          forUse: use,
          with: &builder)
      }
    }

    return designatedInitializer
  }

  private func setSemantics(
    _ semantics: AssignmentSemantics,
    forUse storeUse: Use,
    with builder: inout Builder
  ) {
    switch builder.module[storeUse.userPath] {
    case let user as StoreInst:
      builder.insertionPointer = InsertionPointer(before: storeUse.userPath)
      builder.buildStore(target: user.target, value: user.value, semantics: semantics)
      builder.remove(at: storeUse.userPath)

    case let user as CopyAddrInst:
      builder.insertionPointer = InsertionPointer(before: storeUse.userPath)
      builder.buildCopyAddr(target: user.target, source: user.source, semantics: semantics)
      builder.remove(at: storeUse.userPath)

    default:
      fatalError("not a storage assignment instruction")
    }
  }

}
