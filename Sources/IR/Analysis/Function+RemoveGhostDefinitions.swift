extension Function {

  /// Removes ghost definitions from `self`.
  ///
  /// A ghost definition has no operational semantics and only informs IR analyses and
  /// transformations. They can be ignored for the purpose of code generation.
  mutating func removeGhostDefinitions() {
    for i in instructionIdentities where self[i].isGhost {
      fixUses(of: i)
      remove(i)
    }
  }

  /// Eliminates the ghost `i` from the use-def chain by wiring its uses directly to `i`'s source.
  ///
  /// Postcondition: `i` has no uses.
  private mutating func fixUses(of i: InstructionID) {
    switch self[i] {
    case let x as Access:
      replaceUses(of: .register(i), with: x.source)
    case let x as OpenCapture:
      replaceUses(of: .register(i), with: x.source)
    default:
      let uses = allUses(of: i)
      precondition(
        uses.isEmpty,
        "Instruction \(self[i]) should have no uses, but has uses \(uses.map { self[$0.user] })")
    }
  }

}

extension Instruction {

  /// `true` iff this instruction is a ghost definition.
  fileprivate var isGhost: Bool {
    self is Access || self is EndAccess || self is CloseCapture || self is ReleaseCaptures
      || self is MarkState || self is DeallocStack
  }

}
