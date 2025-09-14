import FrontEnd

extension Module {

  /// Eliminates redundant instructions and fold address computations in `f`.
  public mutating func simplify(_ f: Function.ID) {
    self[f].simplify()
  }

}

extension Function {

  /// Eliminates redundant instructions and fold address computations in `f`.
  public mutating func simplify() {
    for b in blockIDs {
      var i = firstInstruction(in: b)
      while let n = i {
        i = eliminateRedundantAccess(n, of: b)
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
  private mutating func eliminateRedundantAccess(_ i: InstructionID, of b: Block.ID) -> InstructionID? {
    guard
      let s = self[i] as? Access,
      let r = self[s.source] as? Access,
      s.capabilities == r.capabilities, s.binding == nil
    else {
      return instruction(after: i, in: b)
    }

    for u in allUses(of: i) where self[u.user] is EndAccess {
      removeInstruction(u.user)
    }
    replaceUses(of: .register(i), with: s.source)

    defer { removeInstruction(i) }
    return instruction(after: i, in: b)
  }

}
