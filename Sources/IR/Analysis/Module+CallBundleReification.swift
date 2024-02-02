import Core

extension Module {

  /// Replace occurrences of `call_bundle` by `call` depending on the uses of their first operands,
  /// reporting errors and warnings to `diagnostics`.
  ///
  /// - Requires: `f` is in `self`.
  public mutating func reifyCallsToBundles(in f: Function.ID, diagnostics: inout DiagnosticSet) {
    for i in blocks(in: f).map(instructions(in:)).joined() where self[i] is CallBundle {
      reify(callBundle: i, for: .let)
    }
  }

  private mutating func reify(callBundle i: InstructionID, for k: AccessEffect) {
    let s = self[i] as! CallBundle
    assert(s.capabilities.contains(k))

    var arguments = Array(s.arguments)
    let r = makeAccess(k, from: arguments[0], at: s.site)
    arguments[0] = .register(insert(r, before: i))

    let b = Block.ID(containing: i)
    let f = self.reference(to: s.variants[k]!, specializedBy: s.bundle.arguments, in: self[b].scope)

    let reified = makeCall(
      applying: .constant(f), to: arguments, writingResultTo: s.output, at: s.site)
    replace(i, with: reified)
  }

}
