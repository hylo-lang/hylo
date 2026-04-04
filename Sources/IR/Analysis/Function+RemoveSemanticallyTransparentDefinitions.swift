extension Function {

  /// Remove from `self` all the definitions that are semantically transparent for code generation.
  ///
  /// A semantically transparent definition has no operational semantics and only informs
  /// IR analyses and transformations. They can be ignored for the purpose of code generation.
  mutating func removeSemanticallyTransparentDefinitions() {
    // Operate in reverse order to attempt to remove uses of the instructions before removing the instructions themselves.
    for i in instructionIdentities.reversed() where self[i].isSemanticallyTransparent {
      fixUses(of: i)
      remove(i)
    }
  }

  /// Eliminates the semantically transparent `i` from the use-def chain by wiring its uses
  /// directly to `i`'s source.
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

  /// `true` iff this instruction is a semantically transparent definition.
  fileprivate var isSemanticallyTransparent: Bool {
    self is Access || self is EndAccess || self is CloseCapture || self is ReleaseCaptures
      || self is MarkState || self is DeallocStack
  }

}
