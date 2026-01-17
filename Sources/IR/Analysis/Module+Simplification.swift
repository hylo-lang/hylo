import FrontEnd

extension Module {

  /// Eliminates redundant instructions and fold place computations in `f`.
  public mutating func simplify(_ f: Function.ID) {
    for b in self[f].blockIDs {
      var i = self[f].firstInstruction(in: b)
      while let n = i {
        i = eliminateRedundantAccess(n, of: b, in: f)
      }
    }
  }

  /// Replaces uses of `i` by its source if `i` is redundant.
  ///
  /// An access is redundant if its source is another access requesting the same capability. When
  /// that occurs, instructions closing the redundant access can be removed and uses be replaced by
  /// uses of its source.
  ///
  /// Replaces all the uses of `i`, not just the ones in `b`.
  private mutating func eliminateRedundantAccess(_ i: InstructionID, of b: Block.ID, in f: Function.ID) -> InstructionID? {
    guard
      let s = self[i, in: f] as? Access,
      let r = self[s.source, in: f] as? Access,
      s.capabilities == r.capabilities, s.binding == nil
    else {
      return self[f].instruction(after: i, in: b)
    }

    for u in self[f].allUses(of: i) where self[f][u.user] is EndAccess {
      self[f].remove(u.user)
    }
    self[f].replaceUses(of: .register(i), with: s.source)

    defer { self[f].remove(i) }
    return self[f].instruction(after: i, in: b)
  }

}
