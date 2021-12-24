public struct LifetimeAnalyis {

  public init() {}

  public mutating func run(on funName: String, in module: inout Module) {
    // Identify the last uses of borrowing instructions.
    var livenessPass = LivenessAnalysis(isOfInterest: { $0 is BorrowAddrInst })

    // Insert end_borrow_addr for all definitions that are not dominated by a lifetime-ending use.
    for (block, sets) in livenessPass.run(on: funName, in: &module) {
      for def in sets.liveIn.union(sets.defs) where !sets.liveOut.contains(def) {
        var lastUser: InstIndex?
        for user in module.blocks[block].instructions {
          if module.instructions[user].operands.contains(Operand(def)) {
            lastUser = user
          }
        }

        // End the loan if the definition has no use, or if its last user does not consume it.
        let last = lastUser.map({ module.instructions[$0] })
        switch last {
        case is EndBorrowInst:
          continue

        default:
          module.insertEndBorrowAddr(source: Operand(def), at: .after(inst: lastUser ?? def))
        }
      }
    }
  }

}
