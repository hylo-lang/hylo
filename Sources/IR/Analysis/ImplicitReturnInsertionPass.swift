import Core

/// A transformation pass that inserts return instructions.
public struct ImplicitReturnInsertionPass: TransformPass {

  public static let name = "Implicit return insertion"

  public private(set) var diagnostics: [Diagnostic] = []

  /// Creates a new pass.
  public init() {}

  public mutating func run(function f: Function.ID, module: inout Module) -> Bool {
    /// The expected return type of the function.
    let expectedReturnType = module[f].output.astType

    // Reinitialize the internal state of the pass.
    diagnostics.removeAll()

    for b in module[f].blocks.indices {
      let lastInstruction = module[f][b.address].instructions.last
      if let l = lastInstruction, l.isTerminator { continue }

      module.insertReturnVoidInstruction(
        anchoredAt: lastInstruction?.site ?? .empty(at: module[f].anchor),
        at: module.globalEndIndex(of: Block.ID(function: f, address: b.address)),
        inFunctionReturning: expectedReturnType,
        diagnostics: &diagnostics)
    }

    return diagnostics.isEmpty
  }

}

extension Module {

  /// Inserts at `i` an instruction `return void` anchored at `anchor` if `returnType` is `.void`.
  /// Otherwise, writes a diagnostic to `diagnostics`.
  fileprivate mutating func insertReturnVoidInstruction(
    anchoredAt anchor: SourceRange,
    at i: InstructionIndex,
    inFunctionReturning returnType: AnyType,
    diagnostics: inout [Diagnostic]
  ) {
    if returnType == .void {
      insert(ReturnInstruction(site: anchor), at: i)
    } else {
      diagnostics.append(.missingFunctionReturn(expectedReturnType: returnType, at: anchor))
    }
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
