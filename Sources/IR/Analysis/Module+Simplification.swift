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
  private mutating func eliminateRedundantAccess(_ i: AbsoluteInstructionID) -> AbsoluteInstructionID? {
    guard
      let s = self[i] as? Access,
      let r = self[s.source] as? Access,
      s.capabilities == r.capabilities, s.binding == nil
    else {
      return instruction(after: i)
    }

    for u in allUses(of: i) where self[u.user] is EndAccess {
      removeInstruction(u.user)
    }
    replaceUses(of: .register(i), with: s.source, in: i.function)

    defer { removeInstruction(i) }
    return instruction(after: i)
  }

}
