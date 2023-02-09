import Core

/// A transformation pass that inserts return instructions.
public struct ImplicitReturnInsertionPass {

  /// Creates a new pass.
  public init() {}

  /// If `f` returns `Void`, inserts `return` instructions in all basic blocks without a terminator
  /// instruction. Otherwise, report missing return values. `f` is in `module`.
  public func run(function f: Function.ID, module: inout Module, diagnostics: inout Diagnostics) {
    /// The expected return type of the function.
    let expectedReturnType = module[f].output.astType

    for b in module[f].blocks.indices {
      let lastInstruction = module[f][b.address].instructions.last
      if let l = lastInstruction, l.isTerminator { continue }

      module.insertReturnVoidInstruction(
        anchoredAt: lastInstruction?.site ?? .empty(at: module[f].anchor),
        at: module.globalEndIndex(of: Block.ID(function: f, address: b.address)),
        inFunctionReturning: expectedReturnType,
        diagnostics: &diagnostics)
    }
  }

}

extension Module {

  /// Inserts at `i` an instruction `return void` anchored at `anchor` if `returnType` is `.void`.
  /// Otherwise, writes a diagnostic to `diagnostics`.
  fileprivate mutating func insertReturnVoidInstruction(
    anchoredAt anchor: SourceRange,
    at i: InstructionIndex,
    inFunctionReturning returnType: AnyType,
    diagnostics: inout Diagnostics
  ) {
    if returnType == .void {
      insert(ReturnInstruction(site: anchor), at: i)
    } else {
      diagnostics.report(.missingFunctionReturn(expectedReturnType: returnType, at: anchor))
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
