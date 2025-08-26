import FrontEnd

extension Module {

  /// Removes unreachable code from `f`, reporting errors and warnings to `diagnostics`.
  ///
  /// - Requires: `f` is in `self`.
  public mutating func removeDeadCode(in f: Function.ID, diagnostics: inout DiagnosticSet) {
    self[f].removeUnusedDefinitions()
    self[f].removeCodeAfterCallsReturningNever()
    self[f].removeUnreachableBlocks()
  }

}

extension Function {

  /// Removes the instructions if `f` that have no user.
  fileprivate mutating func removeUnusedDefinitions() {
    var s = Set<InstructionID>()
    removeUnused(instructionIDs, keepingTrackIn: &s)
  }

  /// Removes the instructions in `definitions` that have no user, accumulating the IDs of removed
  /// elements in `removed`.
  private mutating func removeUnused<S: Sequence<InstructionID>>(
    _ definitions: S, keepingTrackIn removed: inout Set<InstructionID>
  ) {
    for i in definitions where !removed.contains(i) {
      if allUses(of: i).isEmpty && isRemovableWhenUnused(i) {
        removed.insert(i)
        removeUnused(self[i].operands.compactMap(\.instruction), keepingTrackIn: &removed)
        removeInstruction(i)
      }
    }
  }

  /// Removes the basic blocks that have no predecessor from `f`, except its entry.
  fileprivate mutating func removeUnreachableBlocks() {
    // Nothing to do if there isn't more than one block in the function.
    if blocks.count < 2 { return }

    // Process all blocks except the entry.
    var work = Array(blockIDs.dropFirst())
    var e = work.count
    var changed = true
    while changed {
      // CFG is computed the first time and recomputed every time a mutation happened.
      let cfg = cfg()
      changed = false

      var i = 0
      while i < e {
        if cfg.predecessors(of: work[i]).isEmpty {
          removeBlock(work[i])
          work.swapAt(i, e - 1)
          changed = true
          e -= 1
        } else {
          i += 1
        }
      }
    }
  }

  /// Removes the code after calls returning `Never` from `f`.
  fileprivate mutating func removeCodeAfterCallsReturningNever() {
    for b in blockIDs {
      if let i = instructions(in: b).first(where: { returnsNever($0) }) {
        removeAllInstructions(after: i)
        makeUnreachable(at: self[i].site, insertingAt: .after(i))
      }
    }
  }

  /// Returns `true` iff `i` never returns control flow.
  private func returnsNever(_ i: InstructionID) -> Bool {
    switch self[i] {
    case is Call:
      return type(of: (self[i] as! Call).output).ast.isNever
    case is CallFFI:
      return (self[i] as! CallFFI).returnType.ast.isNever
    default:
      return false
    }
  }

  /// Returns `true` iff `i` can be removed if it has no use.
  private func isRemovableWhenUnused(_ i: InstructionID) -> Bool {
    switch self[i] {
    case let s as Access:
      return s.binding == nil
    case is CallFFI:
      return false
    case let s:
      return s.result != nil
    }
  }

}
