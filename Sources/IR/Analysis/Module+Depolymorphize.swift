import Core
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
      !callee.genericArguments.isEmpty
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
    guard !s.parameterization.isEmpty else { return }

    // TODO: Use existentialization unless the subscript is inlinable

    let g = monomorphize(s.callee, in: ir, for: s.parameterization, usedIn: scope(containing: i))
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
    _ r: FunctionReference, in ir: IR.Program, usedIn useScope: AnyScopeID
  ) -> Function.ID {
    monomorphize(r.function, in: ir, for: r.genericArguments, usedIn: useScope)
  }

  /// Returns a reference to the monomorphized form of `f` for given `parameterization` in
  /// `useScope`, reading definitions from `ir`.
  @discardableResult
  private mutating func monomorphize(
    _ f: Function.ID, in ir: IR.Program,
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
      rewrittenIntructions[i] = InstructionID(b, self[b].instructions.lastAddress!)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(access i: InstructionID, to b: Block.ID) {
      let s = sourceModule[i] as! Access
      append(makeAccess(s.capabilities, from: rewritten(s.source), at: s.site), to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(addressToPointer i: InstructionID, to b: Block.ID) {
      let s = sourceModule[i] as! AddressToPointer
      append(makeAddressToPointer(rewritten(s.source), at: s.site), to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(advancedByBytes i: InstructionID, to b: Block.ID) {
      let s = sourceModule[i] as! AdvancedByBytes
      append(
        makeAdvancedByBytes(source: rewritten(s.base), offset: rewritten(s.byteOffset), at: s.site),
        to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(allocStack i: InstructionID, to b: Block.ID) {
      let s = sourceModule[i] as! AllocStack
      let t = program.monomorphize(s.allocatedType, for: parameterization)
      append(makeAllocStack(t, at: s.site), to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(branch i: InstructionID, to b: Block.ID) {
      let s = sourceModule[i] as! Branch
      append(makeBranch(to: rewrittenBlocks[s.target]!, at: s.site), to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(call i: InstructionID, to b: Block.ID) {
      let s = sourceModule[i] as! Call

      let newCallee: Operand
      if let callee = s.callee.constant as? FunctionReference, !callee.genericArguments.isEmpty {
        let useScope = sourceModule.scope(containing: i)
        let p = program.monomorphize(callee.genericArguments, for: parameterization)
        let g = monomorphize(callee.function, in: ir, for: p, usedIn: useScope)
        newCallee = .constant(FunctionReference(to: g, in: self))
      } else {
        newCallee = rewritten(s.callee)
      }

      let a = s.arguments.map(rewritten(_:))
      let o = rewritten(s.output)
      append(makeCall(applying: newCallee, to: a, writingResultTo: o, at: s.site), to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(callFFI i: InstructionID, to b: Block.ID) {
      let s = sourceModule[i] as! CallFFI
      let t = program.monomorphize(s.returnType, applying: parameterization)
      let o = s.operands.map(rewritten(_:))
      append(makeCallFFI(returning: t, applying: s.callee, to: o, at: s.site), to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(closeUnion i: InstructionID, to b: Block.ID) {
      let s = sourceModule[i] as! CloseUnion
      append(makeCloseUnion(rewritten(s.start), at: s.site), to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(condBranch i: InstructionID, to b: Block.ID) {
      let s = sourceModule[i] as! CondBranch
      let c = rewritten(s.condition)

      let newInstruction = makeCondBranch(
        if: c, then: rewrittenBlocks[s.targetIfTrue]!, else: rewrittenBlocks[s.targetIfFalse]!,
        at: s.site)
      append(newInstruction, to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(deallocStack i: InstructionID, to b: Block.ID) {
      let s = sourceModule[i] as! DeallocStack
      append(makeDeallocStack(for: rewritten(s.location), at: s.site), to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(endBorrow i: InstructionID, to b: Block.ID) {
      let s = sourceModule[i] as! EndAccess
      append(makeEndAccess(rewritten(s.start), at: s.site), to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(endProject i: InstructionID, to b: Block.ID) {
      let s = sourceModule[i] as! EndProject
      append(makeEndProject(rewritten(s.start), at: s.site), to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(globalAddr i: InstructionID, to b: Block.ID) {
      let s = sourceModule[i] as! GlobalAddr
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
      let s = sourceModule[i] as! MarkState
      append(makeMarkState(rewritten(s.storage), initialized: s.initialized, at: s.site), to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(load i: InstructionID, to b: Block.ID) {
      let s = sourceModule[i] as! Load
      append(makeLoad(rewritten(s.source), at: s.site), to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(openUnion i: InstructionID, to b: Block.ID) {
      let s = sourceModule[i] as! OpenUnion
      let u = makeOpenUnion(
        rewritten(s.container), as: program.monomorphize(s.payloadType, for: parameterization),
        forInitialization: s.isUsedForInitialization,
        at: s.site)
      append(u, to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(partialApply i: InstructionID, to b: Block.ID) {
      let s = sourceModule[i] as! PartialApply

      let newCallee: FunctionReference
      if s.callee.genericArguments.isEmpty {
        assert(!sourceModule[s.callee.function].isGeneric)
        newCallee = s.callee
      } else {
        let useScope = sourceModule.scope(containing: i)
        let p = program.monomorphize(s.callee.genericArguments, for: parameterization)
        let g = monomorphize(s.callee.function, in: ir, for: p, usedIn: useScope)
        newCallee = FunctionReference(to: g, in: self)
      }

      let e = rewritten(s.environment)
      append(makePartialApply(wrapping: newCallee, with: e, at: s.site), to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(pointerToAddress i: InstructionID, to b: Block.ID) {
      let s = sourceModule[i] as! PointerToAddress
      let t = program.monomorphize(^s.target, for: parameterization)

      let newInstruction = makePointerToAddress(
        rewritten(s.source), to: RemoteType(t)!, at: s.site)
      append(newInstruction, to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(project i: InstructionID, to b: Block.ID) {
      let s = sourceModule[i] as! Project

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
      let s = sourceModule[i] as! Return
      append(makeReturn(at: s.site), to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(store i: InstructionID, to b: Block.ID) {
      let s = sourceModule[i] as! Store
      append(makeStore(rewritten(s.object), at: rewritten(s.target), at: s.site), to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(subfieldView i: InstructionID, to b: Block.ID) {
      let s = sourceModule[i] as! SubfieldView
      append(
        makeSubfieldView(of: rewritten(s.recordAddress), subfield: s.subfield, at: s.site), to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(switch i: InstructionID, to b: Block.ID) {
      let s = sourceModule[i] as! Switch
      let n = rewritten(s.index)

      let newInstruction = makeSwitch(
        on: n, toOneOf: s.successors.map({ rewrittenBlocks[$0]! }), at: s.site)
      append(newInstruction, to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(unionDiscriminator i: InstructionID, to b: Block.ID) {
      let s = sourceModule[i] as! UnionDiscriminator
      append(makeUnionDiscriminator(rewritten(s.container), at: s.site), to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(unreachable i: InstructionID, to b: Block.ID) {
      let s = sourceModule[i] as! Unreachable
      append(makeUnreachable(at: s.site), to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(yield i: InstructionID, to b: Block.ID) {
      let s = sourceModule[i] as! Yield
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
      case .register(let s):
        return .register(rewrittenIntructions[s]!)
      }
    }
  }

  /// Returns the identity of the Val IR function monomorphizing `f` for given `parameterization`
  /// in `useScope`.
  private mutating func demandMonomorphizedDeclaration(
    of f: Function.ID, in ir: IR.Program,
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

extension TypedProgram {

  /// Returns a copy of `generic` monomorphized for the given `arguments`.
  ///
  /// This method has no effect if `arguments` is empty.
  fileprivate func monomorphize(
    _ generic: IR.`Type`, applying arguments: GenericArguments
  ) -> IR.`Type` {
    let t = monomorphize(generic.ast, for: arguments)
    return .init(ast: t, isAddress: generic.isAddress)
  }

}
