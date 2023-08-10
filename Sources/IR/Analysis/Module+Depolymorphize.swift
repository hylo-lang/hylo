import Core
import FrontEnd
import Utils

extension Module {

  /// Generates the non-parametric resilient API of `self`, reading definitions from `ir`.
  public mutating func depolymorphize(in ir: IR.Program) {
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
  private mutating func depolymorphize(_ f: Function.ID, in ir: IR.Program) {
    for i in blocks(in: f).map(instructions(in:)).joined() {
      switch self[i] {
      case is Call:
        depolymorphize(call: i, in: ir)
      case is Project:
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
  private mutating func depolymorphize(call i: InstructionID, in ir: IR.Program) {
    let s = self[i] as! Call
    guard
      let callee = s.callee.constant as? FunctionReference,
      !callee.specialization.isEmpty
    else { return }

    // TODO: Use existentialization unless the function is inlinable

    let g = monomorphize(callee, in: ir, usedIn: scope(containing: i))
    let r = FunctionReference(to: g, in: self)
    let new = makeCall(
      applying: .constant(r), to: Array(s.arguments), writingResultTo: s.output, at: s.site)
    replace(i, with: new)
  }

  /// If `i` is the projection through a generic subscript, replaces it by an instruction applying
  /// a depolymorphized version of its callee. Otherwise, does nothing.
  ///
  /// - Requires: `i` identifies a `ProjectInstruction`
  private mutating func depolymorphize(project i: InstructionID, in ir: IR.Program) {
    let s = self[i] as! Project
    guard !s.specialization.isEmpty else { return }

    // TODO: Use existentialization unless the subscript is inlinable

    let g = monomorphize(s.callee, in: ir, for: s.specialization, in: scope(containing: i))
    let new = makeProject(
      s.projection, applying: g, specializedBy: [:], to: s.operands, at: s.site)
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

  /// Returns the canonical form of `generic`, specialized for `specialization` in `s`.
  private func monomorphize(
    _ generic: AnyType, for specialization: GenericArguments, in s: AnyScopeID
  ) -> AnyType {
    let t = program.specialize(generic, for: specialization, in: s)
    return program.canonical(t, in: s)
  }

  /// Returns `generic` specialized for `specialization` in `s`.
  private func monomorphize(
    _ generic: IR.`Type`, for specialization: GenericArguments, in s: AnyScopeID
  ) -> IR.`Type` {
    let t = monomorphize(generic.ast, for: specialization, in: s)
    return .init(ast: t, isAddress: generic.isAddress)
  }

  /// Returns the monomorphized form of `r` for use in `s`, reading definitions from `ir`.
  @discardableResult
  private mutating func monomorphize(
    _ r: FunctionReference, in ir: IR.Program, usedIn s: AnyScopeID
  ) -> Function.ID {
    monomorphize(r.function, in: ir, for: r.specialization, in: s)
  }

  /// Returns a reference to the monomorphized form of `f` for `specialization` in `s` reading
  /// definitions from `ir`.
  @discardableResult
  private mutating func monomorphize(
    _ f: Function.ID, in ir: IR.Program,
    for specialization: GenericArguments, in s: AnyScopeID
  ) -> Function.ID {
    let result = demandMonomorphizedDeclaration(of: f, in: ir, for: specialization, in: s)
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
        monomorphize(t, for: specialization, in: scope)
      }
      let target = Block.ID(result, self[result].appendBlock(in: scope, taking: inputs))
      rewrittenBlocks[source] = target

      // Rewrite all instructions from the source block.
      for i in sourceModule[source].instructions.addresses {
        rewrite(InstructionID(source, i), to: .init(target, in: scope))
      }
    }

    return result

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(_ i: InstructionID, to b: ScopedValue<Block.ID>) {
      switch sourceModule[i] {
      case is Access:
        rewrite(access: i, to: b)
      case is AddressToPointer:
        rewrite(addressToPointer: i, to: b)
      case is AdvancedByBytes:
        rewrite(advancedByBytes: i, to: b)
      case is AllocStack:
        rewrite(allocStack: i, to: b)
      case is Branch:
        rewrite(branch: i, to: b)
      case is Call:
        rewrite(call: i, to: b)
      case is CallFFI:
        rewrite(callFFI: i, to: b)
      case is CloseUnion:
        rewrite(closeUnion: i, to: b)
      case is CondBranch:
        rewrite(condBranch: i, to: b)
      case is DeallocStack:
        rewrite(deallocStack: i, to: b)
      case is EndAccess:
        rewrite(endBorrow: i, to: b)
      case is EndProject:
        rewrite(endProject: i, to: b)
      case is GlobalAddr:
        rewrite(globalAddr: i, to: b)
      case is LLVMInstruction:
        rewrite(llvm: i, to: b)
      case is Load:
        rewrite(load: i, to: b)
      case is MarkState:
        rewrite(markState: i, to: b)
      case is OpenUnion:
        rewrite(openUnion: i, to: b)
      case is PartialApply:
        rewrite(partialApply: i, to: b)
      case is PointerToAddress:
        rewrite(pointerToAddress: i, to: b)
      case is Project:
        rewrite(project: i, to: b)
      case is Return:
        rewrite(return: i, to: b)
      case is Store:
        rewrite(store: i, to: b)
      case is SubfieldView:
        rewrite(subfieldView: i, to: b)
      case is Switch:
        rewrite(switch: i, to: b)
      case is UnionDiscriminator:
        rewrite(unionDiscriminator: i, to: b)
      case is Unreachable:
        rewrite(unreachable: i, to: b)
      case is Yield:
        rewrite(yield: i, to: b)
      default:
        fatalError("not implemented")
      }
      rewrittenIntructions[i] = InstructionID(b.value, self[b.value].instructions.lastAddress!)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(access i: InstructionID, to b: ScopedValue<Block.ID>) {
      let s = sourceModule[i] as! Access
      let newInstruction = makeAccess(
        s.capabilities, from: rewritten(s.source, forUseIn: b.scope), at: s.site)
      append(newInstruction, to: b.value)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(addressToPointer i: InstructionID, to b: ScopedValue<Block.ID>) {
      let s = sourceModule[i] as! AddressToPointer
      let newInstruction = makeAddressToPointer(rewritten(s.source, forUseIn: b.scope), at: s.site)
      append(newInstruction, to: b.value)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(advancedByBytes i: InstructionID, to b: ScopedValue<Block.ID>) {
      let s = sourceModule[i] as! AdvancedByBytes
      let u = rewritten(s.base, forUseIn: b.scope)
      let v = rewritten(s.byteOffset, forUseIn: b.scope)
      append(makeAdvancedByBytes(source: u, offset: v, at: s.site), to: b.value)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(allocStack i: InstructionID, to b: ScopedValue<Block.ID>) {
      let s = sourceModule[i] as! AllocStack
      let t = monomorphize(s.allocatedType, for: specialization, in: b.scope)
      append(makeAllocStack(t, at: s.site), to: b.value)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(branch i: InstructionID, to b: ScopedValue<Block.ID>) {
      let s = sourceModule[i] as! Branch
      append(makeBranch(to: rewrittenBlocks[s.target]!, at: s.site), to: b.value)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(call i: InstructionID, to b: ScopedValue<Block.ID>) {
      let s = sourceModule[i] as! Call

      let newCallee: Operand
      if let callee = s.callee.constant as? FunctionReference, !callee.specialization.isEmpty {
        let p = program.specialize(callee.specialization, for: specialization, in: b.scope)
        let g = monomorphize(callee.function, in: ir, for: p, in: b.scope)
        newCallee = .constant(FunctionReference(to: g, in: self))
      } else {
        newCallee = rewritten(s.callee, forUseIn: b.scope)
      }

      let a = s.arguments.map({ rewritten($0, forUseIn: b.scope) })
      let o = rewritten(s.output, forUseIn: b.scope)
      append(makeCall(applying: newCallee, to: a, writingResultTo: o, at: s.site), to: b.value)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(callFFI i: InstructionID, to b: ScopedValue<Block.ID>) {
      let s = sourceModule[i] as! CallFFI
      let t = monomorphize(s.returnType, for: specialization, in: b.scope)
      let o = s.operands.map({ rewritten($0, forUseIn: b.scope) })
      append(makeCallFFI(returning: t, applying: s.callee, to: o, at: s.site), to: b.value)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(closeUnion i: InstructionID, to b: ScopedValue<Block.ID>) {
      let s = sourceModule[i] as! CloseUnion
      append(makeCloseUnion(rewritten(s.start, forUseIn: b.scope), at: s.site), to: b.value)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(condBranch i: InstructionID, to b: ScopedValue<Block.ID>) {
      let s = sourceModule[i] as! CondBranch
      let c = rewritten(s.condition, forUseIn: b.scope)

      let newInstruction = makeCondBranch(
        if: c, then: rewrittenBlocks[s.targetIfTrue]!, else: rewrittenBlocks[s.targetIfFalse]!,
        at: s.site)
      append(newInstruction, to: b.value)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(deallocStack i: InstructionID, to b: ScopedValue<Block.ID>) {
      let s = sourceModule[i] as! DeallocStack
      let newInstruction = makeDeallocStack(
        for: rewritten(s.location, forUseIn: b.scope), at: s.site)
      append(newInstruction, to: b.value)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(endBorrow i: InstructionID, to b: ScopedValue<Block.ID>) {
      let s = sourceModule[i] as! EndAccess
      append(makeEndAccess(rewritten(s.start, forUseIn: b.scope), at: s.site), to: b.value)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(endProject i: InstructionID, to b: ScopedValue<Block.ID>) {
      let s = sourceModule[i] as! EndProject
      append(makeEndProject(rewritten(s.start, forUseIn: b.scope), at: s.site), to: b.value)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(globalAddr i: InstructionID, to b: ScopedValue<Block.ID>) {
      let s = sourceModule[i] as! GlobalAddr
      let t = monomorphize(s.valueType, for: specialization, in: b.scope)
      append(makeGlobalAddr(of: s.id, in: s.container, typed: t, at: s.site), to: b.value)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(llvm i: InstructionID, to b: ScopedValue<Block.ID>) {
      let s = sourceModule[i] as! LLVMInstruction
      let o = s.operands.map({ rewritten($0, forUseIn: b.scope) })
      append(makeLLVM(applying: s.instruction, to: o, at: s.site), to: b.value)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(markState i: InstructionID, to b: ScopedValue<Block.ID>) {
      let s = sourceModule[i] as! MarkState
      let o = rewritten(s.storage, forUseIn: b.scope)
      append(makeMarkState(o, initialized: s.initialized, at: s.site), to: b.value)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(load i: InstructionID, to b: ScopedValue<Block.ID>) {
      let s = sourceModule[i] as! Load
      append(makeLoad(rewritten(s.source, forUseIn: b.scope), at: s.site), to: b.value)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(openUnion i: InstructionID, to b: ScopedValue<Block.ID>) {
      let s = sourceModule[i] as! OpenUnion
      let t = monomorphize(s.payloadType, for: specialization, in: b.scope)
      let c = rewritten(s.container, forUseIn: b.scope)

      let newInstruction = makeOpenUnion(
        c, as: t, forInitialization: s.isUsedForInitialization, at: s.site)
      append(newInstruction, to: b.value)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(partialApply i: InstructionID, to b: ScopedValue<Block.ID>) {
      let s = sourceModule[i] as! PartialApply

      let newCallee: FunctionReference
      if s.callee.specialization.isEmpty {
        assert(!sourceModule[s.callee.function].isGeneric)
        newCallee = s.callee
      } else {
        let p = program.specialize(s.callee.specialization, for: specialization, in: b.scope)
        let g = monomorphize(s.callee.function, in: ir, for: p, in: b.scope)
        newCallee = FunctionReference(to: g, in: self)
      }

      let e = rewritten(s.environment, forUseIn: b.scope)
      append(makePartialApply(wrapping: newCallee, with: e, at: s.site), to: b.value)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(pointerToAddress i: InstructionID, to b: ScopedValue<Block.ID>) {
      let s = sourceModule[i] as! PointerToAddress
      let t = monomorphize(^s.target, for: specialization, in: b.scope)

      let newInstruction = makePointerToAddress(
        rewritten(s.source, forUseIn: b.scope), to: RemoteType(t)!, at: s.site)
      append(newInstruction, to: b.value)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(project i: InstructionID, to b: ScopedValue<Block.ID>) {
      let s = sourceModule[i] as! Project

      let newCallee: Function.ID
      if s.specialization.isEmpty {
        newCallee = s.callee
      } else {
        let p = program.specialize(s.specialization, for: specialization, in: b.scope)
        newCallee = monomorphize(s.callee, in: ir, for: p, in: b.scope)
      }

      let projection = RemoteType(monomorphize(^s.projection, for: specialization, in: b.scope))!
      let a = s.operands.map({ rewritten($0, forUseIn: b.scope) })
      let newInstruction = makeProject(
        projection, applying: newCallee, specializedBy: [:], to: a, at: s.site)
      append(newInstruction, to: b.value)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(return i: InstructionID, to b: ScopedValue<Block.ID>) {
      let s = sourceModule[i] as! Return
      append(makeReturn(at: s.site), to: b.value)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(store i: InstructionID, to b: ScopedValue<Block.ID>) {
      let s = sourceModule[i] as! Store
      let v = rewritten(s.object, forUseIn: b.scope)
      let u = rewritten(s.target, forUseIn: b.scope)
      append(makeStore(v, at: u, at: s.site), to: b.value)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(subfieldView i: InstructionID, to b: ScopedValue<Block.ID>) {
      let s = sourceModule[i] as! SubfieldView
      let a = rewritten(s.recordAddress, forUseIn: b.scope)
      append(makeSubfieldView(of: a, subfield: s.subfield, at: s.site), to: b.value)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(switch i: InstructionID, to b: ScopedValue<Block.ID>) {
      let s = sourceModule[i] as! Switch
      let n = rewritten(s.index, forUseIn: b.scope)

      let newInstruction = makeSwitch(
        on: n, toOneOf: s.successors.map({ rewrittenBlocks[$0]! }), at: s.site)
      append(newInstruction, to: b.value)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(unionDiscriminator i: InstructionID, to b: ScopedValue<Block.ID>) {
      let s = sourceModule[i] as! UnionDiscriminator
      let newInstruction = makeUnionDiscriminator(
        rewritten(s.container, forUseIn: b.scope), at: s.site)
      append(newInstruction, to: b.value)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(unreachable i: InstructionID, to b: ScopedValue<Block.ID>) {
      let s = sourceModule[i] as! Unreachable
      append(makeUnreachable(at: s.site), to: b.value)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(yield i: InstructionID, to b: ScopedValue<Block.ID>) {
      let s = sourceModule[i] as! Yield
      let newInstruction = makeYield(
        s.capability, rewritten(s.projection, forUseIn: b.scope), at: s.site)
      append(newInstruction, to: b.value)
    }

    /// Returns the rewritten form of `c` monomorphized for use in `scopeOfuse`.
    func rewritten(_ c: any Constant, forUseIn scopeOfUse: AnyScopeID) -> any Constant {
      if let t = c as? MetatypeType {
        return MetatypeType(monomorphize(^t, for: specialization, in: scopeOfUse))!
      } else {
        return c
      }
    }

    /// Returns the rewritten form of `o` for use in `scopeOfUse`.
    func rewritten(_ o: Operand, forUseIn scopeOfUse: AnyScopeID) -> Operand {
      switch o {
      case .constant(let c):
        return .constant(rewritten(c, forUseIn: scopeOfUse))
      case .parameter(let b, let i):
        return .parameter(rewrittenBlocks[b]!, i)
      case .register(let s):
        return .register(rewrittenIntructions[s]!)
      }
    }
  }

  /// Returns the IR function monomorphizing `f` for `specialization` in `scopeOfUse`.
  private mutating func demandMonomorphizedDeclaration(
    of f: Function.ID, in ir: IR.Program,
    for specialization: GenericArguments, in scopeOfUse: AnyScopeID
  ) -> Function.ID {
    let result = Function.ID(monomorphized: f, for: specialization)
    if functions[result] != nil { return result }

    let m = ir.modules[ir.module(defining: f)]!
    let source = m[f]

    let inputs = source.inputs.map { (p) in
      let t = monomorphize(p.type.bareType, for: specialization, in: scopeOfUse)
      return Parameter(decl: p.decl, type: ParameterType(p.type.access, t))
    }

    let output = monomorphize(source.output, for: specialization, in: scopeOfUse)

    let entity = Function(
      isSubscript: source.isSubscript,
      site: source.site,
      linkage: .module,
      genericParameters: [],
      inputs: inputs,
      output: output,
      blocks: [])

    addFunction(entity, for: result)
    return result
  }

}
