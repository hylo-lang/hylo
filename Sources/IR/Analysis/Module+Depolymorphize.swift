import Core
import Utils

extension Module {

  /// Generates the non-parametric resilient API of `self`.
  public mutating func depolymorphize() {
    let work = functions.keys
    for k in work {
      // Ignore internal functions and functions without definitions.
      let f = functions[k]!
      if (f.linkage != .external) || (f.entry == nil) { continue }

      // Existentialize public generic functions.
      let j = f.isGeneric ? existentialize(k) : k

      // Depolymorphize all public functions.
      depolymorphize(j)
    }
  }

  /// Replace all uses of parametric types and functions in `f` with their monomorphic or
  /// existential  counterparts.
  private mutating func depolymorphize(_ f: Function.ID) {
    for i in blocks(in: f).map(instructions(in:)).joined() {
      guard
        let s = self[i] as? CallInstruction,
        let callee = s.callee.constant as? FunctionReference,
        !callee.arguments.isEmpty
      else { continue }

      let g = monomorphize(callee)
      let r = FunctionReference(to: g, usedIn: callee.useScope, in: self)
      let newCall = makeCall(applying: .constant(r), to: Array(s.arguments), anchoredAt: s.site)
      replace(i, with: newCall)
    }
  }

  /// Returns a depolymorphized copy of `base` in which parametric parameters have been notionally
  /// replaced by parameters accepting existentials.
  ///
  /// The returned function takes `n` additional parameters where `n` is the length of `arguments`.
  /// For example, assume `base` is defined as the generic function below, which takes two generic
  /// parameters:
  ///
  ///      fun foo<T: P, s: Int>(a: T, b: T[s]) -> T
  ///
  /// Its existentialized form is a function:
  ///
  ///      fun foo_e(a: RawPointer, b, RawPointer, T: WitnessTable, s: Int) -> RawPointer
  ///
  /// The pair `(a, T)` is a notional existential container representing the first argument of the
  /// parametric function. The triple `(a, T, s)` represents the second argument.
  private mutating func existentialize(_ base: Function.ID) -> Function.ID {
    // TODO: Implement me
    return base
  }

  /// Returns a reference to the monomorphized form of `r`.
  @discardableResult
  private mutating func monomorphize(_ r: FunctionReference) -> Function.ID {
    let source = functions[r.function]!
    let inputs = source.inputs.map { (p) in
      let t = program.relations.monomorphize(
        p.type.bareType, applying: r.arguments, in: r.useScope, in: program)
      return Parameter(decl: p.decl, type: ParameterType(p.type.access, t))
    }
    let output = program.relations.monomorphize(
      source.output, applying: r.arguments, in: r.useScope, in: program)

    let result = Function.ID(monomorphized: r.function, for: r.arguments)
    if functions[result] != nil { return result }

    let entity = Function(
      isSubscript: source.isSubscript,
      name: "<\(r.arguments.values), \(r.useScope)>(\(source.name))",
      site: source.site,
      linkage: .module,
      inputs: inputs,
      output: output,
      blocks: [])
    addFunction(entity, for: result)

    // TODO: Copy `source`, replacing occurrences of generics

    return result
  }

}
