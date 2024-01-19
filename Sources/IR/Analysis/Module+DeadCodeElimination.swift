import Core

extension Module {

  /// Removes unreachable code from `f`, reporting errors and warnings to `diagnostics`.
  ///
  /// - Requires: `f` is in `self`.
  public mutating func removeDeadCode(in f: Function.ID, diagnostics: inout DiagnosticSet) {
    removeCodeAfterCallsReturningNever(from: f)
    removeUnreachableBlocks(from: f)
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
          removeBlock(.init(f, work[i]))
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
      if let i = instructions(in: b).first(where: returnsNever) {
        removeAllInstructions(after: i)
        insert(makeUnreachable(at: self[i].site), at: .after(i))
      }
    }
  }

  /// Returns `true` iff `i` never returns control flow.
  private func returnsNever(_ i: InstructionID) -> Bool {
    switch self[i] {
    case is Call:
      return type(of: (self[i] as! Call).output).ast == .never
    case is CallFFI:
      return (self[i] as! CallFFI).returnType.ast == .never
    default:
      return false
    }
  }

}
