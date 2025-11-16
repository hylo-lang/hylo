import FrontEnd

extension Module {

  /// Replace occurrences of `call_bundle` by `call` depending on the uses of their first operands,
  /// reporting errors and warnings to `diagnostics`.
  ///
  /// - Requires: `f` is in `self`.
  public mutating func reifyCallsToBundles(in f: Function.ID, diagnostics: inout DiagnosticSet) {
    for i in self[f].instructions where self[i, in: f] is CallBundle {
      reify(callBundle: i, in: f)
    }
  }

  private mutating func reify(callBundle i: InstructionID, in f: Function.ID) {
    let s = self[i, in: f] as! CallBundle
    let k = s.capabilities.weakest!

    var arguments = Array(s.arguments)
    let r = Access(
      capabilities: [k],
      accessedType: self[f].type(of: arguments[0]),
      source: arguments[0],
      site: s.site)
    arguments[0] = .register(self[f].insert(r, at: .before(i)))

    let b = Block.ID(containing: i)
    let x = FunctionReference(
      to: s.variants[k]!, in: self, specializedBy: s.bundle.arguments, in: self[b, in: f].scope)

    let reified = Call(
      callee: .constant(x), output: s.output, arguments: arguments, site: s.site)
    self[f].replace(i, with: reified)
  }

}
