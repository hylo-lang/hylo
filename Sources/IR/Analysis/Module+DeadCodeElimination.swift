import FrontEnd

extension Module {

  /// Removes unreachable code from `f`, reporting errors and warnings to `diagnostics`.
  ///
  /// - Requires: `f` is in `self`.
  public mutating func removeDeadCode(in f: Function.ID, diagnostics: inout DiagnosticSet) {
    removeUnusedDefinitions(from: f)
    removeCodeAfterCallsReturningNever(from: f)
    removeUnreachableBlocks(from: f)
  }

  /// Removes the instructions if `f` that have no user.
  private mutating func removeUnusedDefinitions(from f: Function.ID) {
    var s = Set<InstructionID>()
    removeUnused(instructions(in: f), keepingTrackIn: &s, in: f)
  }

  /// Removes the instructions in `definitions` that have no user, accumulating the IDs of removed
  /// elements in `removed`.
  private mutating func removeUnused<S: Sequence<InstructionID>>(
    _ definitions: S, keepingTrackIn removed: inout Set<InstructionID>, in f: Function.ID
  ) {
    for i in definitions where !removed.contains(i) {
      if allUses(of: i, in: f).isEmpty && isRemovableWhenUnused(i, in: f) {
        removed.insert(i)
        removeUnused(self[i, in: f].operands.compactMap(\.instruction), keepingTrackIn: &removed, in: f)
        removeInstruction(i, in: f)
      }
    }
  }

  /// Removes the basic blocks that have no predecessor from `f`, except its entry.
  private mutating func removeUnreachableBlocks(from f: Function.ID) {
    // Nothing to do if there isn't more than one block in the function.
    if self[f].blocks.count < 2 { return }

    // Process all blocks except the entry.
    var work = Array(self[f].blocks.addresses.dropFirst())
    var e = work.count
    var changed = true
    while changed {
      // CFG is computed the first time and recomputed every time a mutation happened.
      let cfg = self[f].cfg()
      changed = false

      var i = 0
      while i < e {
        if cfg.predecessors(of: work[i]).isEmpty {
          removeBlock(Block.ID(work[i]), from: f)
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
  private mutating func removeCodeAfterCallsReturningNever(from f: Function.ID) {
    for b in blocks(in: f) {
      if let i = instructions(in: b, of: f).first(where: { returnsNever($0, in: f) }) {
        removeAllInstructions(after: i, in: f)
        insert(makeUnreachable(at: self[i, in: f].site), at: .after(i), in: f)
      }
    }
  }

  /// Returns `true` iff `i` never returns control flow.
  private func returnsNever(_ i: InstructionID, in f: Function.ID) -> Bool {
    switch self[i, in: f] {
    case is Call:
      return type(of: (self[i, in: f] as! Call).output, in: f).ast.isNever
    case is CallFFI:
      return (self[i, in: f] as! CallFFI).returnType.ast.isNever
    default:
      return false
    }
  }

  /// Returns `true` iff `i` can be removed if it has no use.
  private func isRemovableWhenUnused(_ i: InstructionID, in f: Function.ID) -> Bool {
    switch self[i, in: f] {
    case let s as Access:
      return s.binding == nil
    case is CallFFI:
      return false
    case let s:
      return s.result != nil
    }
  }

}
