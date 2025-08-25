import FrontEnd

extension Module {

  /// Replace occurrences of `call_bundle` by `call` depending on the uses of their first operands,
  /// reporting errors and warnings to `diagnostics`.
  ///
  /// - Requires: `f` is in `self`.
  public mutating func reifyCallsToBundles(in f: Function.ID, diagnostics: inout DiagnosticSet) {
    for i in instructions(in: f) where self[i, in: f] is CallBundle {
      reify(callBundle: i, in: f)
    }
  }

  private mutating func reify(callBundle i: InstructionID, in f: Function.ID) {
    let s = self[i, in: f] as! CallBundle
    let k = s.capabilities.weakest!

    var arguments = Array(s.arguments)
    let r = makeAccess([k], from: arguments[0], in: f, at: s.site)
    arguments[0] = .register(insert(r, before: i, in: f))

    let b = Block.ID(containing: i)
    let x = FunctionReference(
      to: s.variants[k]!, in: self, specializedBy: s.bundle.arguments, in: self[b, in: f].scope)

    let reified = makeCall(
      applying: .constant(x), to: arguments, writingResultTo: s.output, in: f, at: s.site)
    replace(i, with: reified, in: f)
  }

}
