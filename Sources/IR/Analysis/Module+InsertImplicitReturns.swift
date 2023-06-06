import Core

extension Module {

  /// If `f` returns `Void`, inserts `return` instructions in all basic blocks without a terminator
  /// instruction. Otherwise, report missing return values to `diagnostics`.
  ///
  /// - Requires: `f` is in `self`.
  public mutating func insertImplicitReturns(in f: Function.ID, diagnostics: inout DiagnosticSet) {
    for blockToProcess in blocks(in: f) {
      let lastInstruction = self[blockToProcess].instructions.last
      if let l = lastInstruction, l is Terminator { continue }

      let site = lastInstruction?.site ?? .empty(at: self[f].site.first())
      appendImplicitReturn(to: blockToProcess, at: site, diagnostics: &diagnostics)
    }
  }

  /// Appends a return instruction anchored at `site` to `b` if its container implements a
  /// subscript or a function returning `Void`. Otherwise, writes a diagnostic to `diagnostics`.
  private mutating func appendImplicitReturn(
    to b: Block.ID,
    at site: SourceRange,
    diagnostics: inout DiagnosticSet
  ) {
    if self[b.function].isSubscript {
      append(makeReturn(at: site), to: b)
    } else if self[b.function].output == .void {
      let x0 = Operand.parameter(entry(of: b.function)!, self[b.function].inputs.count)
      let x1 = append(makeBorrow(.set, from: x0, at: site), to: b)[0]
      append(makeStore(.void, at: x1, at: site), to: b)
      append(makeReturn(at: site), to: b)
    } else {
      diagnostics.insert(
        .missingFunctionReturn(expectedReturnType: self[b.function].output, at: site))
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
