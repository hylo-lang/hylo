/// The lifetime pass of IR analysis.
public struct LifetimePass {

  /// The program being lowered.
  public let program: TypedProgram

  /// The diagnostics collected during the pass.
  private var diagnostics: [Diagnostic] = []

  public init(program: TypedProgram) {
    self.program = program
  }

  /// Runs the pass and returns whether it succeeded without any error.
  public mutating func run(function functionID: Function.ID, module: inout Module) -> Bool {
    // Reset the internal state of the pass.
    diagnostics.removeAll()

    for blockIndex in module[functionID].blocks.indices {
      let block = Block.ID(function: functionID, address: blockIndex.address)

      for inst in module[functionID][block.address].instructions.indices {
        switch module[functionID][block.address][inst.address] {
        case let borrow as BorrowInst:
          // Compute the live-range of the instruction.
          let borrowID = block.result(at: inst.address, index: 0)
          let borrowLifetime = lifetime(of: borrowID, in: module)

          // Delete the borrow if it's never used.
          if borrowLifetime.isEmpty {
            if let decl = borrow.binding {
              diagnostics.append(.unusedBinding(name: program.ast[decl].name, range: borrow.range))
            }
            module[functionID][block.address].instructions.remove(at: inst.address)
            continue
          }

          // Insert `end_borrow` after the instruction's last users.
          for lastUse in borrowLifetime.maximalElements {
            let userBlock = Block.ID(function: functionID, address: lastUse.user.block)
            module.insert(
              EndBorrowInst(borrow: borrowID),
              at: InsertionPoint(after: lastUse.user.address, in: userBlock))
          }

        default:
          break
        }
      }
    }

    return diagnostics.isEmpty
  }

  private func lifetime(of operand: Operand, in module: Module) -> Lifetime {
    // Nothing to do if the operand has no use.
    guard let uses = module.uses[operand] else { return Lifetime(operand: operand) }

    // Compute the live-range of the operand.
    var result = module.liveRange(of: operand, definedIn: operand.block!)

    // Extend the lifetime with that of its borrows.
    for use in uses {
      switch module[use.user.function][use.user.block][use.user.address] {
      case is BorrowInst:
        result = module.extend(
          lifetime: result, with: lifetime(of: .result(inst: use.user, index: 0), in: module))
      default:
        continue
      }
    }

    return result
  }

}

extension Diagnostic {

  fileprivate static func unusedBinding(name: Identifier, range: SourceRange?) -> Diagnostic {
    Diagnostic(
      level: .warning,
      message: "binding '\(name)' was never used",
      location: range?.first(),
      window: range.map({ r in Diagnostic.Window(range: r) }))
  }

}
