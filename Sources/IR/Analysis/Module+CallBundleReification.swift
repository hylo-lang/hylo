import FrontEnd

extension Module {

  /// Replace occurrences of `call_bundle` by `call` depending on the uses of their first operands,
  /// reporting errors and warnings to `diagnostics`.
  ///
  /// - Requires: `f` is in `self`.
  public mutating func reifyCallsToBundles(in f: Function.ID, diagnostics: inout DiagnosticSet) {
    self[f].reifyCallsToBundles(module: self, diagnostics: &diagnostics)
  }

}

extension Function {

  fileprivate mutating func reifyCallsToBundles(module m: Module, diagnostics: inout DiagnosticSet) {
    for i in instructionIDs where self[i] is CallBundle {
      reify(callBundle: i, module: m)
    }
  }

  private mutating func reify(callBundle i: InstructionID, module m: Module) {
    let s = self[i] as! CallBundle
    let k = s.capabilities.weakest!

    var arguments = Array(s.arguments)
    let r = makeAccess([k], from: arguments[0], at: s.site, insertingAt: .before(i))
    arguments[0] = .register(r)

    let b = Block.ID(containing: i)
    let ff = FunctionReference(
      to: s.variants[k]!, in: m, specializedBy: s.bundle.arguments, in: self[b].scope)

    let reified = makeCall(
      applying: .constant(ff), to: arguments, writingResultTo: s.output, at: s.site)
    replace(i, with: reified)
  }

}
