import Core

extension Module {

  /// Replace uses of polymorphic types and functions by their monomorphic or existential
  /// counterparts.
  public mutating func depolymorphize() {
    for (k, f) in functions where f.entry != nil {
      depolymorphize(k)
    }
  }

  /// Replace uses of polymorphic types and functions by their monomorphic or existential
  /// counterparts in `f`.
  private mutating func depolymorphize(_ f: Function.ID) {
    // Iterate over the instructions, look for `call`, substitute if generic.
  }

  /// Returns the monomorphized form of `source` for given `arguments` in `useScope`.
  ///
  /// - Requires `source` is not monomorphized.
  private mutating func monomorphize(
    _ base: Function.ID, for arguments: GenericArguments, in useScope: AnyScopeID
  ) -> Function.ID {
    let source = functions[base]!
    let inputs = source.inputs.map { (p) in
      let t = program.relations.monomorphize(
        p.type.bareType, applying: arguments, in: useScope, in: program)
      return Parameter(decl: p.decl, type: ParameterType(p.type.access, t))
    }
    let output = program.relations.monomorphize(
      source.output, applying: arguments, in: useScope, in: program)

    let result = Function.ID(monomorphized: base, for: arguments)
    if functions[result] != nil { return result }

    let entity = Function(
      isSubscript: source.isSubscript,
      name: "<\(arguments), \(useScope)>(\(source.name))",
      site: source.site,
      linkage: .module,
      inputs: inputs,
      output: output,
      blocks: [])
    addFunction(entity, for: result)

    let dominatorTree = DominatorTree(function: base, cfg: source.cfg(), in: self)
    print(dominatorTree)

    return result
  }

}
