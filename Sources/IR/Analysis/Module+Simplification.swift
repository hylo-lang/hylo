import FrontEnd

extension Module {

  /// Eliminates redundant instructions and fold address computations in `f`.
  public mutating func simplify(_ f: Function.ID) {
    for b in blocks(in: f) {
      var i = instructions(in: b, of: f).first
      while let n = i {
        i = eliminateRedundantAccess(n, in: f)
      }
    }
  }

  /// Replaces uses of `i` by its source if `i` is redundant.
  ///
  /// An access is redundant if its source is another access requesting the same capability. When
  /// that occurs, instructions closing the redundant access can be removed and uses be replaced by
  /// uses of its source.
  private mutating func eliminateRedundantAccess(_ i: InstructionID, in f: Function.ID) -> InstructionID? {
    guard
      let s = self[i, in: f] as? Access,
      let r = self[s.source, in: f] as? Access,
      s.capabilities == r.capabilities, s.binding == nil
    else {
      return instruction(after: i, in: f)
    }

    for u in allUses(of: i, in: f) where self[u.user, in: f] is EndAccess {
      removeInstruction(u.user, in: f)
    }
    replaceUses(of: .register(i), with: s.source, in: f)

    defer { removeInstruction(i, in: f) }
    return instruction(after: i, in: f)
  }

}
