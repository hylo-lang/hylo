import Core

extension Module {

  /// Removes unreachable code from `f`, reporting errors and warnings to `diagnostics`.
  ///
  /// - Requires: `f` is in `self`.
  public mutating func removeDeadCode(in f: Function.ID, diagnostics: inout DiagnosticSet) {
    var cfg: ControlFlowGraph
    var changed = false

    repeat {
      cfg = self[f].cfg()
      changed = false

      for b in blocks(in: f) where b.address != self[f].entry {
        if cfg.predecessors(of: b.address).isEmpty {
          removeBlock(b)
          changed = true
          continue
        }

        changed = removeDeadCode(in: b)
      }
    } while changed
  }

  /// Removes unreachable code from `b`, reporting errors and warnings to `diagnostics` and
  /// returning `true` iff a changed occurred.
  private mutating func removeDeadCode(in b: Block.ID) -> Bool {
    let t = terminator(of: b)
    for i in instructions(in: b) where i != t {
      switch self[i] {
      case is Unreachable:
        removeAllInstructionsAfter(i)
        return true

      default:
        continue
      }
    }

    // No change occurred.
    return false
  }

}
