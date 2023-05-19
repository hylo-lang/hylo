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
        program.monomorphize(t, applying: r.arguments)
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
      case is AddressToPointerInstruction:
        rewrite(addressToPointer: i, to: b)
      case is AllocStackInstruction:
        rewrite(allocStack: i, to: b)
      case is BorrowInstruction:
        rewrite(borrow: i, to: b)
      case is BranchInstruction:
        rewrite(branch: i, to: b)
      case is CallInstruction:
        rewrite(call: i, to: b)
      case is CallFFIInstruction:
        rewrite(callFFI: i, to: b)
      case is CondBranchInstruction:
        rewrite(condBranch: i, to: b)
      case is DeallocStackInstruction:
        rewrite(deallocStack: i, to: b)
      case is DeinitInstruction:
        break  // TODO: Should not be necessary
      case is DestructureInstruction:
        rewrite(destructure: i, to: b)
      case is ElementAddrInstruction:
        rewrite(elementAddr: i, to: b)
      case is EndBorrowInstruction:
        rewrite(endBorrow: i, to: b)
      case is EndProjectInstruction:
        rewrite(endBorrow: i, to: b)
      case is GlobalAddrInstruction:
        rewrite(globalAddr: i, to: b)
      case is LLVMInstruction:
        rewrite(llvm: i, to: b)
      case is LoadInstruction:
        rewrite(load: i, to: b)
      case is PointerToAddressInstruction:
        rewrite(pointerToAddress: i, to: b)
      case is RecordInstruction:
        rewrite(record: i, to: b)
      case is ReturnInstruction:
        rewrite(return: i, to: b)
      case is StoreInstruction:
        rewrite(store: i, to: b)
      case is UnrechableInstruction:
        rewrite(unreachable: i, to: b)
      case is YieldInstruction:
        rewrite(yield: i, to: b)
      default:
        fatalError("not implemented")
      }
      rewrittenIntructions[i] = InstructionID(b, self[b].instructions.lastAddress!)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(addressToPointer i: InstructionID, to b: Block.ID) {
      let s = self[i] as! AddressToPointerInstruction
      append(makeAddressToPointer(rewritten(s.source), anchoredAt: s.site), to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(allocStack i: InstructionID, to b: Block.ID) {
      let s = self[i] as! AllocStackInstruction
      let t = program.monomorphize(s.allocatedType, for: r.arguments)
      append(makeAllocStack(t, anchoredAt: s.site), to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(borrow i: InstructionID, to b: Block.ID) {
      let s = self[i] as! BorrowInstruction
      append(makeBorrow(s.capability, from: rewritten(s.location), anchoredAt: s.site), to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(branch i: InstructionID, to b: Block.ID) {
      let s = self[i] as! BranchInstruction
      append(makeBranch(to: rewrittenBlocks[s.target]!, anchoredAt: s.site), to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(call i: InstructionID, to b: Block.ID) {
      let s = self[i] as! CallInstruction

      let newCallee: Operand
      if let callee = s.callee.constant as? FunctionReference, !callee.arguments.isEmpty {
        let g = monomorphize(callee)
        newCallee = .constant(FunctionReference(to: g, usedIn: callee.useScope, in: self))
      } else {
        newCallee = rewritten(s.callee)
      }

      let a = s.arguments.map(rewritten(_:))
      append(makeCall(applying: newCallee, to: a, anchoredAt: s.site), to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(callFFI i: InstructionID, to b: Block.ID) {
      let s = self[i] as! CallFFIInstruction
      let t = program.monomorphize(s.returnType, applying: r.arguments)
      let o = s.operands.map(rewritten(_:))
      append(makeCallFFI(returning: t, applying: s.callee, to: o, anchoredAt: s.site), to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(condBranch i: InstructionID, to b: Block.ID) {
      let s = self[i] as! CondBranchInstruction
      let c = rewritten(s.condition)

      let newInstruction = makeCondBranch(
        if: c, then: rewrittenBlocks[s.targetIfTrue]!, else: rewrittenBlocks[s.targetIfFalse]!,
        anchoredAt: s.site)
      append(newInstruction, to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(deallocStack i: InstructionID, to b: Block.ID) {
      let s = self[i] as! DeallocStackInstruction
      append(makeDeallocStack(for: rewritten(s.location), anchoredAt: s.site), to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(destructure i: InstructionID, to b: Block.ID) {
      let s = self[i] as! DestructureInstruction
      append(makeDestructure(rewritten(s.whole), anchoredAt: s.site), to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(elementAddr i: InstructionID, to b: Block.ID) {
      let s = self[i] as! ElementAddrInstruction
      append(makeElementAddr(rewritten(s.base), at: s.elementPath, anchoredAt: s.site), to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(endBorrow i: InstructionID, to b: Block.ID) {
      let s = self[i] as! EndBorrowInstruction
      append(makeEndBorrow(rewritten(s.borrow), anchoredAt: s.site), to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(endProject i: InstructionID, to b: Block.ID) {
      let s = self[i] as! EndProjectInstruction
      append(makeEndProject(rewritten(s.projection), anchoredAt: s.site), to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(globalAddr i: InstructionID, to b: Block.ID) {
      let s = self[i] as! GlobalAddrInstruction
      let t = program.monomorphize(s.valueType, for: r.arguments)
      append(makeGlobalAddr(of: s.id, in: s.container, typed: t, anchoredAt: s.site), to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(llvm i: InstructionID, to b: Block.ID) {
      let s = self[i] as! LLVMInstruction
      let o = s.operands.map(rewritten(_:))
      append(makeLLVM(applying: s.instruction, to: o, anchoredAt: s.site), to: b)
    }

    func rewrite(load i: InstructionID, to b: Block.ID) {
      let s = self[i] as! LoadInstruction
      append(makeLoad(rewritten(s.source), anchoredAt: s.site), to: b)
    }

    func rewrite(pointerToAddress i: InstructionID, to b: Block.ID) {
      let s = self[i] as! PointerToAddressInstruction
      let t = program.monomorphize(^s.target, for: r.arguments)

      let newInstruction = makePointerToAddress(
        rewritten(s.source), to: RemoteType(t)!, anchoredAt: s.site)
      append(newInstruction, to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(record i: InstructionID, to b: Block.ID) {
      let s = self[i] as! RecordInstruction
      let t = program.monomorphize(s.objectType.ast, for: r.arguments)
      let o = s.operands.map(rewritten(_:))
      append(makeRecord(t, aggregating: o, anchoredAt: s.site), to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(return i: InstructionID, to b: Block.ID) {
      let s = self[i] as! ReturnInstruction
      append(makeReturn(rewritten(s.object), anchoredAt: s.site), to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(store i: InstructionID, to b: Block.ID) {
      let s = self[i] as! StoreInstruction
      append(makeStore(rewritten(s.object), at: rewritten(s.target), anchoredAt: s.site), to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(unreachable i: InstructionID, to b: Block.ID) {
      let s = self[i] as! UnrechableInstruction
      append(makeUnreachable(anchoredAt: s.site), to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(yield i: InstructionID, to b: Block.ID) {
      let s = self[i] as! YieldInstruction
      append(makeYield(s.capability, rewritten(s.projection), anchoredAt: s.site), to: b)
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
      let t = program.monomorphize(p.type.bareType, for: r.arguments)
      return Parameter(decl: p.decl, type: ParameterType(p.type.access, t))
    }

    let output = program.monomorphize(source.output, for: r.arguments)

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

  /// Returns a copy of `generic` monomorphized for the given `arguments`.
  ///
  /// This method has no effect if `arguments` is empty.
  fileprivate func monomorphize(
    _ generic: LoweredType, applying arguments: GenericArguments
  ) -> LoweredType {
    let t = monomorphize(generic.ast, for: arguments)
    return .init(ast: t, isAddress: generic.isAddress)
  }

}
