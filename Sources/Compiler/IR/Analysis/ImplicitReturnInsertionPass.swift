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

    for i in module[functionID].blocks.indices {
      if module[functionID][i.address].instructions.last?.isTerminator ?? false  {
        // There's a terminator instruction. Move to the next block.
        continue
      } else if expectedReturnType == .unit {
        // Insert missing return instruction.
        let ip = InsertionPoint(endOf: Block.ID(function: functionID, address: i.address))
        module.insert(ReturnInst(), at: ip)
      } else {
        // No return instruction, yet the function must return a non-unit value.
        let range = module[functionID][i.address].instructions
          .last(where: { $0.range != nil })?.range
        diagnostics.append(.missingFunctionReturn(
          expectedReturnType: expectedReturnType, range: range))
      }
    }

    return diagnostics.isEmpty
  }

}

fileprivate extension Diagnostic {

  static func missingFunctionReturn(
    expectedReturnType: Type,
    range: SourceRange?
  ) -> Diagnostic {
    Diagnostic(
      level: .error,
      message: "missing return in function expected to return '\(expectedReturnType)'",
      location: range?.first(),
      window: range.map({ r in Diagnostic.Window(range: r) }))
  }

}
