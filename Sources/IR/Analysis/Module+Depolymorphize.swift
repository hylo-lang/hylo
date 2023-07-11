import Core
import Utils

extension Module {

  /// Generates the non-parametric resilient API of `self`, reading definitions from `ir`.
  public mutating func depolymorphize(in ir: LoweredProgram) {
    let work = functions.keys
    for k in work {
      // Ignore internal functions and functions without definitions.
      let f = functions[k]!
      if (f.linkage != .external) || (f.entry == nil) { continue }

      if f.isGeneric {
        // Existentialize public, non-inlinable generic functions.
        _ = existentialize(k)
      } else {
        depolymorphize(k, in: ir)
      }
    }
  }

  /// Replaces uses of parametric types and functions in `f` with their monomorphic or existential
  /// counterparts, reading definitions from `ir`.
  private mutating func depolymorphize(_ f: Function.ID, in ir: LoweredProgram) {
    for i in blocks(in: f).map(instructions(in:)).joined() {
      switch self[i] {
      case is CallInstruction:
        depolymorphize(call: i, in: ir)
      case is ProjectInstruction:
        depolymorphize(project: i, in: ir)
      default:
        continue
      }
    }
  }

  /// If `i` is a call to a generic function, replaces it by an instruction applying a
  /// depolymorphized version of its callee. Otherwise, does nothing.
  ///
  /// - Requires: `i` identifies a `CallInstruction`
  private mutating func depolymorphize(call i: InstructionID, in ir: LoweredProgram) {
    let s = self[i] as! CallInstruction
    guard
      let callee = s.callee.constant as? FunctionReference,
      !callee.arguments.isEmpty
    else { return }

    // TODO: Use existentialization unless the function is inlinable

    let g = monomorphize(callee, in: ir)
    let r = FunctionReference(to: g, usedIn: callee.useScope, in: self)
    let new = makeCall(
      applying: .constant(r), to: Array(s.arguments), writingResultTo: s.output, at: s.site)
    replace(i, with: new)
  }

  /// If `i` is the projection through a generic subscript, replaces it by an instruction applying
  /// a depolymorphized version of its callee. Otherwise, does nothing.
  ///
  /// - Requires: `i` identifies a `ProjectInstruction`
  private mutating func depolymorphize(project i: InstructionID, in ir: LoweredProgram) {
    let s = self[i] as! ProjectInstruction
    guard !s.parameterization.isEmpty else { return }

    // TODO: Use existentialization unless the subscript is inlinable

    let useScope = self[Block.ID(i.function, i.block)].scope
    let g = monomorphize(s.callee, in: ir, for: s.parameterization, usedIn: useScope)
    let new = makeProject(
      s.projection, applying: g, parameterizedBy: [:], to: s.operands, at: s.site)
    replace(i, with: new)
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

  /// Returns a reference to the monomorphized form of `r`, reading definitions from `ir`.
  @discardableResult
  private mutating func monomorphize(
    _ r: FunctionReference, in ir: LoweredProgram
  ) -> Function.ID {
    monomorphize(r.function, in: ir, for: r.arguments, usedIn: r.useScope)
  }

  /// Returns a reference to the monomorphized form of `f` for given `parameterization` in
  /// `useScope`, reading definitions from `ir`.
  @discardableResult
  private mutating func monomorphize(
    _ f: Function.ID, in ir: LoweredProgram,
    for parameterization: GenericArguments, usedIn useScope: AnyScopeID
  ) -> Function.ID {
    let result = demandMonomorphizedDeclaration(
      of: f, in: ir, for: parameterization, usedIn: useScope)
    if self[result].entry != nil {
      return result
    }

    var rewrittenBlocks: [Block.ID: Block.ID] = [:]
    var rewrittenIntructions: [InstructionID: InstructionID] = [:]

    // Iterate over the basic blocks of the source function in a way that guarantees we always
    // visit definitions before their uses.
    let sourceModule = ir.modules[ir.module(defining: f)]!
    let cfg = sourceModule[f].cfg()
    let sourceBlocks = DominatorTree(function: f, cfg: cfg, in: sourceModule).bfs

    for b in sourceBlocks {
      let source = Block.ID(f, b)

      // Rewrite the source block in the monomorphized function.
      let scope = sourceModule[source].scope
      let inputs = sourceModule[source].inputs.map { (t) in
        program.monomorphize(t, applying: parameterization)
      }
      let target = Block.ID(result, self[result].appendBlock(in: scope, taking: inputs))
      rewrittenBlocks[source] = target

      // Rewrite all instructions from the source block.
      for i in sourceModule[source].instructions.addresses {
        rewrite(InstructionID(source, i), to: target)
      }
    }

    return result

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(_ i: InstructionID, to b: Block.ID) {
      switch sourceModule[i] {
      case is AddressToPointerInstruction:
        rewrite(addressToPointer: i, to: b)
      case is AdvancedByBytesInstruction:
        rewrite(advancedByBytes: i, to: b)
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
      case is CloseSumInstruction:
        rewrite(closeSum: i, to: b)
      case is CondBranchInstruction:
        rewrite(condBranch: i, to: b)
      case is DeallocStackInstruction:
        rewrite(deallocStack: i, to: b)
      case is EndBorrowInstruction:
        rewrite(endBorrow: i, to: b)
      case is EndProjectInstruction:
        rewrite(endProject: i, to: b)
      case is GlobalAddrInstruction:
        rewrite(globalAddr: i, to: b)
      case is LLVMInstruction:
        rewrite(llvm: i, to: b)
      case is LoadInstruction:
        rewrite(load: i, to: b)
      case is MarkStateInstruction:
        rewrite(markState: i, to: b)
      case is OpenSumInstruction:
        rewrite(openSum: i, to: b)
      case is PartialApplyInstruction:
        rewrite(partialApply: i, to: b)
      case is PointerToAddressInstruction:
        rewrite(pointerToAddress: i, to: b)
      case is ProjectInstruction:
        rewrite(project: i, to: b)
      case is ReturnInstruction:
        rewrite(return: i, to: b)
      case is StoreInstruction:
        rewrite(store: i, to: b)
      case is SubfieldViewInstruction:
        rewrite(subfieldView: i, to: b)
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
      let s = sourceModule[i] as! AddressToPointerInstruction
      append(makeAddressToPointer(rewritten(s.source), at: s.site), to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(advancedByBytes i: InstructionID, to b: Block.ID) {
      let s = sourceModule[i] as! AdvancedByBytesInstruction
      append(
        makeAdvancedByBytes(source: rewritten(s.base), offset: rewritten(s.byteOffset), at: s.site),
        to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(allocStack i: InstructionID, to b: Block.ID) {
      let s = sourceModule[i] as! AllocStackInstruction
      let t = program.monomorphize(s.allocatedType, for: parameterization)
      append(makeAllocStack(t, at: s.site), to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(borrow i: InstructionID, to b: Block.ID) {
      let s = sourceModule[i] as! BorrowInstruction
      append(makeBorrow(s.capability, from: rewritten(s.location), at: s.site), to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(branch i: InstructionID, to b: Block.ID) {
      let s = sourceModule[i] as! BranchInstruction
      append(makeBranch(to: rewrittenBlocks[s.target]!, at: s.site), to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(call i: InstructionID, to b: Block.ID) {
      let s = sourceModule[i] as! CallInstruction

      let newCallee: Operand
      if let callee = s.callee.constant as? FunctionReference, !callee.arguments.isEmpty {
        let p = program.monomorphize(callee.arguments, for: parameterization)
        let g = monomorphize(callee.function, in: ir, for: p, usedIn: callee.useScope)
        newCallee = .constant(FunctionReference(to: g, usedIn: callee.useScope, in: self))
      } else {
        newCallee = rewritten(s.callee)
      }

      let a = s.arguments.map(rewritten(_:))
      let o = rewritten(s.output)
      append(makeCall(applying: newCallee, to: a, writingResultTo: o, at: s.site), to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(callFFI i: InstructionID, to b: Block.ID) {
      let s = sourceModule[i] as! CallFFIInstruction
      let t = program.monomorphize(s.returnType, applying: parameterization)
      let o = s.operands.map(rewritten(_:))
      append(makeCallFFI(returning: t, applying: s.callee, to: o, at: s.site), to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(closeSum i: InstructionID, to b: Block.ID) {
      let s = sourceModule[i] as! CloseSumInstruction
      append(makeCloseSum(rewritten(s.start), at: s.site), to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(condBranch i: InstructionID, to b: Block.ID) {
      let s = sourceModule[i] as! CondBranchInstruction
      let c = rewritten(s.condition)

      let newInstruction = makeCondBranch(
        if: c, then: rewrittenBlocks[s.targetIfTrue]!, else: rewrittenBlocks[s.targetIfFalse]!,
        at: s.site)
      append(newInstruction, to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(deallocStack i: InstructionID, to b: Block.ID) {
      let s = sourceModule[i] as! DeallocStackInstruction
      append(makeDeallocStack(for: rewritten(s.location), at: s.site), to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(endBorrow i: InstructionID, to b: Block.ID) {
      let s = sourceModule[i] as! EndBorrowInstruction
      append(makeEndBorrow(rewritten(s.borrow), at: s.site), to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(endProject i: InstructionID, to b: Block.ID) {
      let s = sourceModule[i] as! EndProjectInstruction
      append(makeEndProject(rewritten(s.projection), anchoredAt: s.site), to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(globalAddr i: InstructionID, to b: Block.ID) {
      let s = sourceModule[i] as! GlobalAddrInstruction
      let t = program.monomorphize(s.valueType, for: parameterization)
      append(makeGlobalAddr(of: s.id, in: s.container, typed: t, at: s.site), to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(llvm i: InstructionID, to b: Block.ID) {
      let s = sourceModule[i] as! LLVMInstruction
      let o = s.operands.map(rewritten(_:))
      append(makeLLVM(applying: s.instruction, to: o, at: s.site), to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(markState i: InstructionID, to b: Block.ID) {
      let s = sourceModule[i] as! MarkStateInstruction
      append(makeMarkState(rewritten(s.storage), initialized: s.initialized, at: s.site), to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(load i: InstructionID, to b: Block.ID) {
      let s = sourceModule[i] as! LoadInstruction
      append(makeLoad(rewritten(s.source), at: s.site), to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(openSum i: InstructionID, to b: Block.ID) {
      let s = sourceModule[i] as! OpenSumInstruction
      let u = makeOpenSum(
        rewritten(s.container), as: program.monomorphize(s.payloadType, for: parameterization),
        forInitialization: s.isUsedForInitialization,
        at: s.site)
      append(u, to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(partialApply i: InstructionID, to b: Block.ID) {
      let s = sourceModule[i] as! PartialApplyInstruction

      let newCallee: FunctionReference
      if s.callee.arguments.isEmpty {
        assert(!sourceModule[s.callee.function].isGeneric)
        newCallee = s.callee
      } else {
        let p = program.monomorphize(s.callee.arguments, for: parameterization)
        let g = monomorphize(s.callee.function, in: ir, for: p, usedIn: s.callee.useScope)
        newCallee = FunctionReference(to: g, usedIn: s.callee.useScope, in: self)
      }

      let e = rewritten(s.environment)
      append(makePartialApply(wrapping: newCallee, with: e, at: s.site), to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(pointerToAddress i: InstructionID, to b: Block.ID) {
      let s = sourceModule[i] as! PointerToAddressInstruction
      let t = program.monomorphize(^s.target, for: parameterization)

      let newInstruction = makePointerToAddress(
        rewritten(s.source), to: RemoteType(t)!, at: s.site)
      append(newInstruction, to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(project i: InstructionID, to b: Block.ID) {
      let s = sourceModule[i] as! ProjectInstruction

      let newCallee: Function.ID
      if s.parameterization.isEmpty {
        newCallee = s.callee
      } else {
        let p = program.monomorphize(s.parameterization, for: parameterization)
        newCallee = monomorphize(s.callee, in: ir, for: p, usedIn: self[b].scope)
      }

      let t = RemoteType(program.monomorphize(^s.projection, for: s.parameterization))!
      let a = s.operands.map(rewritten(_:))
      append(
        makeProject(t, applying: newCallee, parameterizedBy: [:], to: a, at: s.site), to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(return i: InstructionID, to b: Block.ID) {
      let s = sourceModule[i] as! ReturnInstruction
      append(makeReturn(at: s.site), to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(store i: InstructionID, to b: Block.ID) {
      let s = sourceModule[i] as! StoreInstruction
      append(makeStore(rewritten(s.object), at: rewritten(s.target), at: s.site), to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(subfieldView i: InstructionID, to b: Block.ID) {
      let s = sourceModule[i] as! SubfieldViewInstruction
      append(
        makeSubfieldView(of: rewritten(s.recordAddress), subfield: s.subfield, at: s.site), to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(unreachable i: InstructionID, to b: Block.ID) {
      let s = sourceModule[i] as! UnrechableInstruction
      append(makeUnreachable(at: s.site), to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(yield i: InstructionID, to b: Block.ID) {
      let s = sourceModule[i] as! YieldInstruction
      append(makeYield(s.capability, rewritten(s.projection), at: s.site), to: b)
    }

    /// Returns the rewritten form of `c` for use in `result`.
    func rewritten(_ c: any Constant) -> any Constant {
      if let t = c as? MetatypeType {
        return MetatypeType(program.monomorphize(^t, for: parameterization))!
      } else {
        return c
      }
    }

    /// Returns the rewritten form of `o`, which is used in `r.function`, for use in `result`.
    func rewritten(_ o: Operand) -> Operand {
      switch o {
      case .constant(let c):
        return .constant(rewritten(c))
      case .parameter(let b, let i):
        return .parameter(rewrittenBlocks[b]!, i)
      case .register(let s, let i):
        return .register(rewrittenIntructions[s]!, i)
      }
    }
  }

  /// Returns the identity of the Val IR function monomorphizing `f` for given `parameterization`
  /// in `useScope`.
  private mutating func demandMonomorphizedDeclaration(
    of f: Function.ID, in ir: LoweredProgram,
    for parameterization: GenericArguments, usedIn useScope: AnyScopeID
  ) -> Function.ID {
    let result = Function.ID(monomorphized: f, for: parameterization)
    if functions[result] != nil { return result }

    let m = ir.modules[ir.module(defining: f)]!
    let source = m[f]

    let inputs = source.inputs.map { (p) in
      let t = program.monomorphize(p.type.bareType, for: parameterization)
      return Parameter(decl: p.decl, type: ParameterType(p.type.access, t))
    }

    let output = program.monomorphize(source.output, for: parameterization)

    let entity = Function(
      isSubscript: source.isSubscript,
      name: "<\(list: parameterization.values), \(useScope)>(\(source.name))",
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
