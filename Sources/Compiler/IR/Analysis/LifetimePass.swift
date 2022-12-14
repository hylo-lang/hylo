/// The lifetime pass.
public struct LifetimePass: TransformPass {

  public static let name = "Lifetime"

  /// The program being lowered.
  public let program: TypedProgram

  public private(set) var diagnostics: [Diagnostic] = []

  public init(program: TypedProgram) {
    self.program = program
  }

  public mutating func run(function functionID: Function.ID, module: inout Module) -> Bool {
    // Reinitialize the internal state of the pass.
    diagnostics.removeAll()

    for blockIndex in module[function: functionID].blocks.indices {
      let block = Block.ID(function: functionID, address: blockIndex.address)

      for instruction in module[block: block].instructions.indices {
        switch module[block: block][instruction.address] {
        case let borrow as BorrowInstruction:
          // Compute the live-range of the instruction.
          let borrowID = block.result(at: instruction.address, index: 0)
          let borrowLifetime = lifetime(of: borrowID, in: module)

          // Delete the borrow if it's never used.
          if borrowLifetime.isEmpty {
            if let decl = borrow.binding {
              diagnostics.append(.unusedBinding(name: program.ast[decl].name, at: borrow.range))
            }
            module[block: block].instructions.remove(at: instruction.address)
            continue
          }

          // Insert `end_borrow` after the instruction's last users.
          for lastUse in borrowLifetime.maximalElements {
            module.insert(EndBorrowInstruction(borrow: borrowID, range: nil), after: lastUse.user)
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
      switch module[instruction: use.user] {
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

  fileprivate static func unusedBinding(name: Identifier, at range: SourceRange?) -> Diagnostic {
    .warning("binding '\(name)' was never used", range: range)
  }

}
