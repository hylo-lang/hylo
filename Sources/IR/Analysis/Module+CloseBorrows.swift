import Core

extension Module {

  /// Inserts `end_borrow` instructions after the last use of each `borrow` instruction in `f`,
  /// reporting errors and warnings to `diagnostics`.
  ///
  /// - Requires: `f` is in `self`.
  public mutating func closeBorrows(in f: Function.ID, diagnostics: inout DiagnosticSet) {
    for blockToProcess in blocks(in: f) {
      for i in instructions(in: blockToProcess) {
        switch self[i] {
        case let borrow as Borrow:
          let result = Operand.register(i)
          let borrowLifetime = lifetime(of: result)

          // Delete the borrow if it's never used.
          if borrowLifetime.isEmpty {
            if let decl = borrow.binding {
              diagnostics.insert(.unusedBinding(name: program.ast[decl].baseName, at: borrow.site))
            }
            removeInstruction(i)
            continue
          }

          // Insert `end_borrow` after the instruction's last users.
          for lastUse in borrowLifetime.maximalElements() {
            let s = makeEndBorrow(result, at: self[lastUse.user].site)
            insert(s, after: lastUse.user)
          }

        case is Project:
          // Insert `end_project` after the instruction's last users.
          let result = Operand.register(i)
          for lastUse in lifetime(of: result).maximalElements() {
            let s = makeEndProject(result, anchoredAt: self[lastUse.user].site)
            insert(s, after: lastUse.user)
          }

        default:
          break
        }
      }
    }
  }

  private func lifetime(of operand: Operand) -> Lifetime {
    // Nothing to do if the operand has no use.
    guard let uses = uses[operand] else { return Lifetime(operand: operand) }

    // Compute the live-range of the operand.
    var r = liveRange(of: operand, definedIn: operand.block!)

    // Extend the lifetime with that of its borrows.
    for use in uses {
      switch self[use.user] {
      case is LifetimeExtender:
        if let o = result(of: use.user) {
          r = extend(lifetime: r, with: lifetime(of: o))
        }

      default:
        continue
      }
    }

    return r
  }

}

extension Diagnostic {

  fileprivate static func unusedBinding(name: Identifier, at site: SourceRange) -> Diagnostic {
    .warning("binding '\(name)' was never used", at: site)
  }

}

/// An instruction that extends the lifetime of all its uses.
private protocol LifetimeExtender {}

extension Borrow: LifetimeExtender {}

extension OpenUnion: LifetimeExtender {}

extension Project: LifetimeExtender {}

extension ProjectBundle: LifetimeExtender {}

extension SubfieldView: LifetimeExtender {}

extension WrapExistentialAddr: LifetimeExtender {}
