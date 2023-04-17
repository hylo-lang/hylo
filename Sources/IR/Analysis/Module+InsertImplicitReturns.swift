import Core

extension Module {

  /// If `f` returns `Void`, inserts `return` instructions in all basic blocks without a terminator
  /// instruction. Otherwise, report missing return values to `diagnostics`.
  ///
  /// - Requires: `f` is in `self`.
  public mutating func insertImplicitReturns(in f: Function.ID, diagnostics: inout DiagnosticSet) {
    /// The expected return type of the function.
    let expectedReturnType = self[f].output.ast

    for blockToProcess in blocks(in: f) {
      let lastInstruction = self[blockToProcess].instructions.last
      if let l = lastInstruction, l is Terminator { continue }

      insertReturnVoidInstruction(
        anchoredAt: lastInstruction?.site ?? .empty(at: self[f].anchor),
        at: endIndex(of: blockToProcess),
        inFunctionReturning: expectedReturnType,
        diagnostics: &diagnostics)
    }
  }

  /// Inserts at `i` an instruction `return void` anchored at `anchor` if `returnType` is `.void`.
  /// Otherwise, writes a diagnostic to `diagnostics`.
  private mutating func insertReturnVoidInstruction(
    anchoredAt anchor: SourceRange,
    at i: InstructionIndex,
    inFunctionReturning returnType: AnyType,
    diagnostics: inout DiagnosticSet
  ) {
    if program.relations.areEquivalent(returnType, .void) {
      insert(makeReturn(.constant(.void), anchoredAt: anchor), at: i)
    } else {
      diagnostics.insert(.missingFunctionReturn(expectedReturnType: returnType, at: anchor))
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
