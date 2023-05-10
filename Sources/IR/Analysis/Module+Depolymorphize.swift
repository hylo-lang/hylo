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
    let result = demandMonomorphizedDeclaration(r)
    if self[result].entry != nil {
      return result
    }

    var rewrittenBlocks: [Block.ID: Block.ID] = [:]
    var rewrittenIntructions: [InstructionID: InstructionID] = [:]

    // Iterate over the basic blocks of the source function in a way that guarantees we always
    // visit definitions before their uses.
    let dominatorTree = DominatorTree(function: r.function, cfg: self[r.function].cfg(), in: self)
    for b in dominatorTree.bfs {
      let source = Block.ID(r.function, b)

      // Rewrite the source block in the monomorphized function.
      let inputs = self[source].inputs.map { (t) in
        program.monomorphize(t, applying: r.arguments, in: r.useScope)
      }
      let target = Block.ID(result, self[result].appendBlock(taking: inputs))
      rewrittenBlocks[source] = target

      // Rewrite all instructions from the source block.
      for i in self[source].instructions.addresses {
        rewrite(InstructionID(source, i), to: target)
      }
    }

    return result

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(_ i: InstructionID, to b: Block.ID) {
      switch self[i] {
      case is RecordInstruction:
        rewrite(record: i, to: b)
      case is ReturnInstruction:
        rewrite(return: i, to: b)
      default:
        fatalError("not implemented")
      }
      rewrittenIntructions[i] = InstructionID(b, self[b].instructions.lastAddress!)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(return i: InstructionID, to b: Block.ID) {
      let s = self[i] as! ReturnInstruction
      append(makeReturn(rewritten(s.object), anchoredAt: s.site), to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(record i: InstructionID, to b: Block.ID) {
      let s = self[i] as! RecordInstruction
      let t = program.monomorphize(s.objectType.ast, applying: r.arguments, in: r.useScope)
      let o = s.operands.map(rewritten(_:))
      append(makeRecord(t, aggregating: o, anchoredAt: s.site), to: b)
    }

    /// Returns the rewritten form of `o`, which is used in `r.function`, for use in `result`.
    func rewritten(_ o: Operand) -> Operand {
      switch o {
      case .constant:
        return o
      case .parameter(let b, let i):
        return .parameter(rewrittenBlocks[b]!, i)
      case .register(let s, let i):
        return .register(rewrittenIntructions[s]!, i)
      }
    }
  }

  /// Returns the identity of the Val IR function monomorphizing the function referred to by `r`.
  private mutating func demandMonomorphizedDeclaration(_ r: FunctionReference) -> Function.ID {
    let result = Function.ID(monomorphized: r.function, for: r.arguments)
    if functions[result] != nil { return result }

    let source = self[r.function]

    let inputs = source.inputs.map { (p) in
      let t = program.monomorphize(p.type.bareType, applying: r.arguments, in: r.useScope)
      return Parameter(decl: p.decl, type: ParameterType(p.type.access, t))
    }

    let output = program.relations.monomorphize(
      source.output, applying: r.arguments, in: r.useScope, in: program)

    let entity = Function(
      isSubscript: source.isSubscript,
      name: "<\(list: r.arguments.values), \(r.useScope)>(\(source.name))",
      site: source.site,
      linkage: .module,
      parameters: [],
      inputs: inputs,
      output: output,
      blocks: [])

    addFunction(entity, for: result)
    return result
  }

}

extension TypedProgram {

  /// Returns a copy of `generic` where occurrences of parameters keying `subtitutions` are
  /// replaced by their corresponding value, performing necessary conformance lookups from
  /// `useScope`, which is in `self`.
  ///
  /// This method has no effect if `substitutions` is empty.
  fileprivate func monomorphize(
    _ generic: LoweredType,
    applying substitutions: GenericArguments,
    in useScope: AnyScopeID
  ) -> LoweredType {
    let t = monomorphize(generic.ast, applying: substitutions, in: useScope)
    return .init(ast: t, isAddress: generic.isAddress)
  }

}
