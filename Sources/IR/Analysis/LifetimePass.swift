import Core

extension Module {

  /// Inserts `end_borrow` instructions after the last use of each `borrow` instruction in `f`,
  /// reporting errors and warnings to `diagnostics`.
  ///
  /// - Requires: `f` is in `self`.
  public mutating func closeBorrows(in f: Function.ID, diagnostics: inout DiagnosticSet) {
    for blockIndex in self[f].blocks.indices {
      let block = Block.ID(function: f, address: blockIndex.address)

      for instruction in self[block].instructions.indices {
        switch self[block][instruction.address] {
        case let borrow as BorrowInstruction:
          // Compute the live-range of the instruction.
          let borrowID = block.result(at: instruction.address, index: 0)
          let borrowLifetime = lifetime(of: borrowID, in: self)

          // Delete the borrow if it's never used.
          if borrowLifetime.isEmpty {
            if let decl = borrow.binding {
              diagnostics.insert(.unusedBinding(name: decl.baseName, at: borrow.site))
            }
            self[block].instructions.remove(at: instruction.address)
            continue
          }

          // Insert `end_borrow` after the instruction's last users.
          for lastUse in borrowLifetime.maximalElements {
            insert(
              makeEndBorrow(borrowID, anchoredAt: self[lastUse.user].site),
              after: lastUse.user)
          }

        default:
          break
        }
      }
    }
  }

  private func lifetime(of operand: Operand, in module: Module) -> Lifetime {
    // Nothing to do if the operand has no use.
    guard let uses = module.uses[operand] else { return Lifetime(operand: operand) }

    // Compute the live-range of the operand.
    var result = module.liveSite(of: operand, definedIn: operand.block!)

    // Extend the lifetime with that of its borrows.
    for use in uses {
      switch self[use.user] {
      case is BorrowInstruction:
        result = module.extend(
          lifetime: result, with: lifetime(of: .result(instruction: use.user, index: 0), in: module)
        )
      default:
        continue
      }
    }

    return result
  }

}

extension Diagnostic {

  fileprivate static func unusedBinding(name: Identifier, at site: SourceRange) -> Diagnostic {
    .warning("binding '\(name)' was never used", at: site)
  }

}
