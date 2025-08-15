import FrontEnd

extension Module {

  /// Eliminates redundant instructions and fold address computations in `f`.
  public mutating func simplify(_ f: Function.ID) {
    for b in self[f].blockIDs {
      var i = self[f].instructions(in: b).first
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
      return self[f].instruction(after: i)
    }

    for u in self[f].allUses(of: i) where self[f][u.user] is EndAccess {
      self[f].removeInstruction(u.user)
    }
    self[f].replaceUses(of: .register(i), with: s.source)

    defer { self[f].removeInstruction(i) }
    return self[f].instruction(after: i)
  }

}
