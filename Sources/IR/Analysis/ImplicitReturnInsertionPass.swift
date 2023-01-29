import Core

/// A transformation pass that inserts return instructions.
public struct ImplicitReturnInsertionPass: TransformPass {

  public static let name = "Implicit return insertion"

  public private(set) var diagnostics: [Diagnostic] = []

  /// Creates a new pass.
  public init() {}

  public mutating func run(function functionID: Function.ID, module: inout Module) -> Bool {
    /// The expected return type of the function.
    let expectedReturnType = module[functionID].output.astType

    // Reinitialize the internal state of the pass.
    diagnostics.removeAll()

    for block in module[functionID].blocks.indices {
      // FIXME: Remove empty blocks
      let last = module[functionID][block.address].instructions.last!

      // Nothing to do if there's a terminator instruction.
      if last.isTerminator { continue }

      if expectedReturnType == .void {
        // Insert missing return instruction.
        module.insert(
          ReturnInstruction(site: last.site),
          at: module.globalEndIndex(of: Block.ID(function: functionID, address: block.address)))
      } else {
        // No return instruction, yet the function must return a non-void value.
        diagnostics.append(
          .missingFunctionReturn(expectedReturnType: expectedReturnType, at: last.site))
      }
    }

    return diagnostics.isEmpty
  }

}

extension Diagnostic {

  fileprivate static func missingFunctionReturn(
    expectedReturnType: AnyType,
    at site: SourceRange
  ) -> Diagnostic {
    .error("missing return in function expected to return '\(expectedReturnType)'", at: site)
  }

}
