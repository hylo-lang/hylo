import FrontEnd

extension Module {

  /// Eliminates redundant instructions and fold address computations in `f`.
  public mutating func simplify(_ f: Function.ID) {
    for b in blocks(in: f) {
      var i = instructions(in: b).first
      while let n = i {
        i = eliminateRedundantAccess(n)
      }
    }
  }

  /// Replaces uses of `i` by its source if `i` is redundant.
  ///
  /// An access is redundant if its source is another access requesting the same capability. When
  /// that occurs, instructions closing the redundant access can be removed and uses be replaced by
  /// uses of its source.
  private mutating func eliminateRedundantAccess(_ i: InstructionID) -> InstructionID? {
    guard
      let s = self[i] as? Access,
      let r = self[s.source] as? Access,
      s.capabilities == r.capabilities, s.binding == nil
    else {
      return instruction(after: i)
    }

    let uses = allUses(of: i).filter({ self[$0.user] is EndAccess })
    modifyIR(of: i.function) { (w) in
      for u in uses {
        w.removeInstruction(u.user)
      }
      w.replaceUses(of: .register(i), with: s.source, in: i.function)
    }

    defer {
      modifyIR(of: i.function) { (w) in
        w.removeInstruction(i)
      }
    }
    return instruction(after: i)
  }

}
