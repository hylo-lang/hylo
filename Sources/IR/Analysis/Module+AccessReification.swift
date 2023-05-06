import Core
import DequeModule
import Utils

extension Module {

  /// Replace uses of `access` instructions by either `borrow` or `load` depending on the weakest
  /// capability required on the access, reporting errors and warnings to `diagnostics`.
  ///
  /// This pass uses def-use chains to identify the weakest capability required on an `access`. If
  /// it is `sink`, the instruction is removed and its uses are replaced be uses of its source.
  /// Otherwise, a `borrow` instruction is substitutde for the access.
  ///
  /// - Requires: `f` is in `self`.
  public mutating func reifyAccesses(in f: Function.ID, diagnostics: inout DiagnosticSet) {
    var work: Deque<InstructionID> = []
    for i in blocks(in: f).map(instructions(in:)).joined() where self[i] is AccessInstruction {
      work.append(i)
    }

    while let i = work.popFirst() {
      let s = self[i] as! AccessInstruction

      var requests: AccessEffectSet = [s.capabilities.weakest!]
      forEachClient(of: i) { (j) in
        switch self[j] {
        case let t as AccessInstruction:
          requests.formUnion(s.capabilities.intersection(t.capabilities))
        case let t as BorrowInstruction:
          if s.capabilities.contains(t.capability) {
            requests = [requests.strongest(including: t.capability)]
          }
        case is LoadInstruction:
          if s.capabilities.contains(.sink) {
            requests = [requests.strongest(including: .sink)]
          }
        default:
          unreachable()
        }
      }

      if let k = requests.uniqueElement {
        reify(i, as: k)
      } else {
        tighten(i, to: requests)
        work.append(i)
      }
    }
  }

  /// Calls `action` on the instructions that use a capability on the access at the origin of `i`.
  private func forEachClient(of i: InstructionID, _ action: (InstructionID) -> Void) {
    guard let uses = self.uses[.register(i, 0)] else { return }
    for u in uses {
      if self[u.user] is ElementAddrInstruction {
        forEachClient(of: u.user, action)
      } else {
        action(u.user)
      }
    }
  }

  /// Replaces the uses of `i` with uses of a borrow or load instruction for capability `k`.
  private mutating func reify(_ i: InstructionID, as k: AccessEffect) {
    let s = self[i] as! AccessInstruction
    if k == .sink {
      replaceUses(of: .register(i, 0), with: s.source, in: i.function)
      removeInstruction(i)
    } else {
      let t = makeBorrow(k, from: s.source, correspondingTo: s.binding, anchoredAt: s.site)
      replace(i, with: t)
    }
  }

  /// Replaces `i` by an access instruction requesting capabilities `k`.
  private mutating func tighten(_ i: InstructionID, to k: AccessEffectSet) {
    let s = self[i] as! AccessInstruction
    let t = makeAccess(k, from: s.source, correspondingTo: s.binding, anchoredAt: s.site)
    replace(i, with: t)
  }

}
