import Core
import FrontEnd
import OrderedCollections
import Utils

extension Module {

  /// Generates the non-parametric resilient API of `self`, reading definitions from `ir`.
  public mutating func depolymorphize(in ir: IR.Program) {
    for k in functions.keys {
      let f = functions[k]!

      // Ignore declarations without definition.
      if f.entry == nil { continue }

      // All non-generic functions are deploymorphized.
      if !f.isGeneric {
        depolymorphize(k, in: ir)
        continue
      }

      // Public generic functions are existentialized.
      if f.linkage == .external {
        _ = existentialize(k)
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

  /// Iff `i` is a call to a generic function, replaces it by an instruction applying a
  /// depolymorphized version of its callee.
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

  /// Iff `i` is the projection through a generic subscript, replaces it by an instruction applying
  /// a depolymorphized version of its callee.
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

  /// Returns the canonical form of `generic`, specialized for `specialization` in `scopeOfUse`.
  private func monomorphize(
    _ generic: AnyType, for specialization: GenericArguments, in scopeOfUse: AnyScopeID
  ) -> AnyType {
    let t = program.specialize(generic, for: specialization, in: scopeOfUse)
    return program.canonical(t, in: scopeOfUse)
  }

  /// Returns `generic` specialized for `specialization` in `scopeOfUse`.
  private func monomorphize(
    _ generic: IR.`Type`, for specialization: GenericArguments, in scopeOfUse: AnyScopeID
  ) -> IR.`Type` {
    let t = monomorphize(generic.ast, for: specialization, in: scopeOfUse)
    return .init(ast: t, isAddress: generic.isAddress)
  }

  /// Returns the monomorphized form of `r` for use in `scopeOfUse`, reading definitions from `ir`.
  @discardableResult
  private mutating func monomorphize(
    _ r: FunctionReference, in ir: IR.Program, usedIn scopeOfUse: AnyScopeID
  ) -> Function.ID {
    monomorphize(r.function, in: ir, for: r.specialization, in: scopeOfUse)
  }

  /// Returns a reference to the monomorphized form of `f` for `specialization` in `scopeOfUse`,
  /// reading definitions from `ir`.
  @discardableResult
  private mutating func monomorphize(
    _ f: Function.ID, in ir: IR.Program,
    for specialization: GenericArguments, in scopeOfUse: AnyScopeID
  ) -> Function.ID {
    let result = demandMonomorphizedDeclaration(of: f, in: ir, for: specialization, in: scopeOfUse)
    if self[result].entry != nil {
      return result
    }

    let sourceModule = ir.modules[ir.module(defining: f)]!
    var rewrittenBlocks: [Block.ID: Block.ID] = [:]
    var rewrittenInstructions: [InstructionID: InstructionID] = [:]

    for b in sourceModule[f].blocks.addresses {
      let source = Block.ID(f, b)
      let inputs = sourceModule[source].inputs.map { (t) in
        monomorphize(t, for: specialization, in: scopeOfUse)
      }
      rewrittenBlocks[source] = Block.ID(
        result,
        self[result].appendBlock(in: sourceModule[source].scope, taking: inputs))
    }

    /// A table from generic value parameter to the allocation of the storage containing its value.
    ///
    /// Generic value parameters are rewritten as local variables initialized at the beginning of
    /// the monomorphized function. Generic value arguments don't require deinitialization. Their
    /// local storage is deallocated before each rewritten return instruction.
    let rewrittenGenericValues = defineGenericValueArguments(specialization, in: result)

    // Iterate over the basic blocks of the source function in a way that guarantees we always
    // visit definitions before their uses.
    let cfg = sourceModule[f].cfg()
    let sourceBlocks = DominatorTree(function: f, cfg: cfg, in: sourceModule).bfs
    for b in sourceBlocks {
      let source = Block.ID(f, b)
      let target = rewrittenBlocks[source]!
      for i in sourceModule[source].instructions.addresses {
        rewrite(InstructionID(source, i), to: target)
      }
    }

    return result

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(_ i: InstructionID, to b: Block.ID) {
      switch sourceModule[i] {
      case is Access:
        rewrittenInstructions[i] = rewrite(access: i, to: b)
      case is AddressToPointer:
        rewrittenInstructions[i] = rewrite(addressToPointer: i, to: b)
      case is AdvancedByBytes:
        rewrittenInstructions[i] = rewrite(advancedByBytes: i, to: b)
      case is AdvancedByStrides:
        rewrittenInstructions[i] = rewrite(advancedByStrides: i, to: b)
      case is AllocStack:
        rewrittenInstructions[i] = rewrite(allocStack: i, to: b)
      case is Branch:
        rewrittenInstructions[i] = rewrite(branch: i, to: b)
      case is Call:
        rewrittenInstructions[i] = rewrite(call: i, to: b)
      case is CallFFI:
        rewrittenInstructions[i] = rewrite(callFFI: i, to: b)
      case is CaptureIn:
        rewrittenInstructions[i] = rewrite(captureIn: i, to: b)
      case is CloseCapture:
        rewrittenInstructions[i] = rewrite(closeUnion: i, to: b)
      case is CloseUnion:
        rewrittenInstructions[i] = rewrite(closeUnion: i, to: b)
      case is CondBranch:
        rewrittenInstructions[i] = rewrite(condBranch: i, to: b)
      case is ConstantString:
        rewrittenInstructions[i] = rewrite(constantString: i, to: b)
      case is DeallocStack:
        rewrittenInstructions[i] = rewrite(deallocStack: i, to: b)
      case is EndAccess:
        rewrittenInstructions[i] = rewrite(endBorrow: i, to: b)
      case is EndProject:
        rewrittenInstructions[i] = rewrite(endProject: i, to: b)
      case is GenericParameter:
        rewrittenInstructions[i] = rewrite(genericParameter: i, to: b)
      case is GlobalAddr:
        rewrittenInstructions[i] = rewrite(globalAddr: i, to: b)
      case is LLVMInstruction:
        rewrittenInstructions[i] = rewrite(llvm: i, to: b)
      case is Load:
        rewrittenInstructions[i] = rewrite(load: i, to: b)
      case is MarkState:
        rewrittenInstructions[i] = rewrite(markState: i, to: b)
      case is MemoryCopy:
        rewrittenInstructions[i] = rewrite(memoryCopy: i, to: b)
      case is OpenCapture:
        rewrittenInstructions[i] = rewrite(openCapture: i, to: b)
      case is OpenUnion:
        rewrittenInstructions[i] = rewrite(openUnion: i, to: b)
      case is PointerToAddress:
        rewrittenInstructions[i] = rewrite(pointerToAddress: i, to: b)
      case is Project:
        rewrittenInstructions[i] = rewrite(project: i, to: b)
      case is ReleaseCaptures:
        rewrittenInstructions[i] = rewrite(releaseCaptures: i, to: b)
      case is Return:
        rewrittenInstructions[i] = rewrite(return: i, to: b)
      case is Store:
        rewrittenInstructions[i] = rewrite(store: i, to: b)
      case is SubfieldView:
        rewrittenInstructions[i] = rewrite(subfieldView: i, to: b)
      case is Switch:
        rewrittenInstructions[i] = rewrite(switch: i, to: b)
      case is UnionDiscriminator:
        rewrittenInstructions[i] = rewrite(unionDiscriminator: i, to: b)
      case is Unreachable:
        rewrittenInstructions[i] = rewrite(unreachable: i, to: b)
      case is Yield:
        rewrittenInstructions[i] = rewrite(yield: i, to: b)
      default:
        UNIMPLEMENTED()
      }
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(access i: InstructionID, to b: Block.ID) -> InstructionID {
      let s = sourceModule[i] as! Access
      let newInstruction = makeAccess(s.capabilities, from: rewritten(s.source), at: s.site)
      return append(newInstruction, to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(addressToPointer i: InstructionID, to b: Block.ID) -> InstructionID {
      let s = sourceModule[i] as! AddressToPointer
      let newInstruction = makeAddressToPointer(rewritten(s.source), at: s.site)
      return append(newInstruction, to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(advancedByBytes i: InstructionID, to b: Block.ID) -> InstructionID {
      let s = sourceModule[i] as! AdvancedByBytes
      let u = rewritten(s.base)
      let v = rewritten(s.byteOffset)
      return append(makeAdvancedByBytes(source: u, offset: v, at: s.site), to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(advancedByStrides i: InstructionID, to b: Block.ID) -> InstructionID {
      let s = sourceModule[i] as! AdvancedByStrides
      let u = rewritten(s.base)
      return append(makeAdvanced(u, byStrides: s.offset, at: s.site), to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(allocStack i: InstructionID, to b: Block.ID) -> InstructionID {
      let s = sourceModule[i] as! AllocStack
      let t = monomorphize(s.allocatedType, for: specialization, in: scopeOfUse)
      return append(makeAllocStack(t, at: s.site), to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(branch i: InstructionID, to b: Block.ID) -> InstructionID {
      let s = sourceModule[i] as! Branch
      return append(makeBranch(to: rewrittenBlocks[s.target]!, at: s.site), to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(call i: InstructionID, to b: Block.ID) -> InstructionID {
      let s = sourceModule[i] as! Call
      let f = rewritten(s.callee)
      let a = s.arguments.map(rewritten(_:))
      let o = rewritten(s.output)
      return append(makeCall(applying: f, to: a, writingResultTo: o, at: s.site), to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(callFFI i: InstructionID, to b: Block.ID) -> InstructionID {
      let s = sourceModule[i] as! CallFFI
      let t = monomorphize(s.returnType, for: specialization, in: scopeOfUse)
      let o = s.operands.map(rewritten(_:))
      return append(makeCallFFI(returning: t, applying: s.callee, to: o, at: s.site), to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(captureIn i: InstructionID, to b: Block.ID) -> InstructionID {
      let s = sourceModule[i] as! CaptureIn
      let newInstruction = makeCapture(rewritten(s.source), in: rewritten(s.target), at: s.site)
      return append(newInstruction, to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(closeCapture i: InstructionID, to b: Block.ID) -> InstructionID {
      let s = sourceModule[i] as! CloseCapture
      return append(makeCloseCapture(rewritten(s.start), at: s.site), to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(closeUnion i: InstructionID, to b: Block.ID) -> InstructionID {
      let s = sourceModule[i] as! CloseUnion
      return append(makeCloseUnion(rewritten(s.start), at: s.site), to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(condBranch i: InstructionID, to b: Block.ID) -> InstructionID {
      let s = sourceModule[i] as! CondBranch
      let c = rewritten(s.condition)

      let newInstruction = makeCondBranch(
        if: c, then: rewrittenBlocks[s.targetIfTrue]!, else: rewrittenBlocks[s.targetIfFalse]!,
        at: s.site)
      return append(newInstruction, to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(constantString i: InstructionID, to b: Block.ID) -> InstructionID {
      let s = sourceModule[i] as! ConstantString
      return append(makeConstantString(utf8: s.value, at: s.site), to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(deallocStack i: InstructionID, to b: Block.ID) -> InstructionID {
      let s = sourceModule[i] as! DeallocStack
      let newInstruction = makeDeallocStack(for: rewritten(s.location), at: s.site)
      return append(newInstruction, to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(endBorrow i: InstructionID, to b: Block.ID) -> InstructionID {
      let s = sourceModule[i] as! EndAccess
      return append(makeEndAccess(rewritten(s.start), at: s.site), to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(endProject i: InstructionID, to b: Block.ID) -> InstructionID {
      let s = sourceModule[i] as! EndProject
      return append(makeEndProject(rewritten(s.start), at: s.site), to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(genericParameter i: InstructionID, to b: Block.ID) -> InstructionID {
      let s = sourceModule[i] as! GenericParameter
      return rewrittenGenericValues[s.parameter]!
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(globalAddr i: InstructionID, to b: Block.ID) -> InstructionID {
      let s = sourceModule[i] as! GlobalAddr
      return append(makeGlobalAddr(of: s.binding, at: s.site), to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(llvm i: InstructionID, to b: Block.ID) -> InstructionID {
      let s = sourceModule[i] as! LLVMInstruction
      let o = s.operands.map(rewritten(_:))
      return append(makeLLVM(applying: s.instruction, to: o, at: s.site), to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(markState i: InstructionID, to b: Block.ID) -> InstructionID {
      let s = sourceModule[i] as! MarkState
      let o = rewritten(s.storage)
      return append(makeMarkState(o, initialized: s.initialized, at: s.site), to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(memoryCopy i: InstructionID, to b: Block.ID) -> InstructionID {
      let s = sourceModule[i] as! MemoryCopy
      let source = rewritten(s.source)
      let target = rewritten(s.target)
      return append(makeMemoryCopy(source, target, at: s.site), to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(load i: InstructionID, to b: Block.ID) -> InstructionID {
      let s = sourceModule[i] as! Load
      return append(makeLoad(rewritten(s.source), at: s.site), to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(openCapture i: InstructionID, to b: Block.ID) -> InstructionID {
      let s = sourceModule[i] as! OpenCapture
      let newInstruction = makeOpenCapture(rewritten(s.source), at: s.site)
      return append(newInstruction, to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(openUnion i: InstructionID, to b: Block.ID) -> InstructionID {
      let s = sourceModule[i] as! OpenUnion
      let t = monomorphize(s.payloadType, for: specialization, in: scopeOfUse)
      let c = rewritten(s.container)
      let newInstruction = makeOpenUnion(
        c, as: t, forInitialization: s.isUsedForInitialization, at: s.site)
      return append(newInstruction, to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(pointerToAddress i: InstructionID, to b: Block.ID) -> InstructionID {
      let s = sourceModule[i] as! PointerToAddress
      let t = monomorphize(^s.target, for: specialization, in: scopeOfUse)
      let newInstruction = makePointerToAddress(
        rewritten(s.source), to: RemoteType(t)!, at: s.site)
      return append(newInstruction, to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(project i: InstructionID, to b: Block.ID) -> InstructionID {
      let s = sourceModule[i] as! Project

      let newCallee: Function.ID
      if s.specialization.isEmpty {
        newCallee = s.callee
      } else {
        newCallee = rewritten(s.callee, specializedBy: s.specialization)
      }

      let projection = RemoteType(
        monomorphize(^s.projection, for: specialization, in: scopeOfUse))!
      let a = s.operands.map(rewritten(_:))
      let newInstruction = makeProject(
        projection, applying: newCallee, specializedBy: [:], to: a, at: s.site)
      return append(newInstruction, to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(releaseCaptures i: InstructionID, to b: Block.ID) -> InstructionID {
      let s = sourceModule[i] as! ReleaseCaptures
      let newInstruction = makeReleaseCapture(rewritten(s.container), at: s.site)
      return append(newInstruction, to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(return i: InstructionID, to b: Block.ID) -> InstructionID {
      let s = sourceModule[i] as! Return

      for i in rewrittenGenericValues.values.reversed() {
        append(makeDeallocStack(for: .register(i), at: s.site), to: b)
      }
      return append(makeReturn(at: s.site), to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(store i: InstructionID, to b: Block.ID) -> InstructionID {
      let s = sourceModule[i] as! Store
      let v = rewritten(s.object)
      let u = rewritten(s.target)
      return append(makeStore(v, at: u, at: s.site), to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(subfieldView i: InstructionID, to b: Block.ID) -> InstructionID {
      let s = sourceModule[i] as! SubfieldView
      let a = rewritten(s.recordAddress)
      let newInstruction = makeSubfieldView(of: a, subfield: s.subfield, at: s.site)
      return append(newInstruction, to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(switch i: InstructionID, to b: Block.ID) -> InstructionID {
      let s = sourceModule[i] as! Switch
      let n = rewritten(s.index)
      let newInstruction = makeSwitch(
        on: n, toOneOf: s.successors.map({ rewrittenBlocks[$0]! }), at: s.site)
      return append(newInstruction, to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(unionDiscriminator i: InstructionID, to b: Block.ID) -> InstructionID {
      let s = sourceModule[i] as! UnionDiscriminator
      let newInstruction = makeUnionDiscriminator(rewritten(s.container), at: s.site)
      return append(newInstruction, to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(unreachable i: InstructionID, to b: Block.ID) -> InstructionID {
      let s = sourceModule[i] as! Unreachable
      return append(makeUnreachable(at: s.site), to: b)
    }

    /// Rewrites `i`, which is in `r.function`, into `result`, at the end of `b`.
    func rewrite(yield i: InstructionID, to b: Block.ID) -> InstructionID {
      let s = sourceModule[i] as! Yield
      let newInstruction = makeYield(s.capability, rewritten(s.projection), at: s.site)
      return append(newInstruction, to: b)
    }

    /// Returns a monomorphized copy of `c` monomorphized for use in `scopeOfuse`.
    func rewritten(_ c: any Constant) -> any Constant {
      switch c {
      case let r as FunctionReference:
        return rewritten(r)
      case let t as MetatypeType:
        return MetatypeType(monomorphize(^t, for: specialization, in: scopeOfUse))!
      default:
        return c
      }
    }

    /// Returns a monomorphized copy of `c` monomorphized for use in `scopeOfuse`.
    func rewritten(_ c: FunctionReference) -> FunctionReference {
      // Unspecialized references cannot refer to trait members, which are specialized for the
      // implicit `Self` parameter.
      if c.specialization.isEmpty { return c }

      let f = rewritten(c.function, specializedBy: c.specialization)
      return FunctionReference(to: f, in: self)
    }

    /// Returns a monomorphized copy of `f` specialized by `z` for use in `scopeOfUse`.
    ///
    /// If `f` is a trait requirement, the result is a monomorphized version of that requirement's
    /// implementation, using `a` to identify the requirement's receiver. Otherwise, the result is
    /// a monomorphized copy of `f`.
    func rewritten(_ f: Function.ID, specializedBy z: GenericArguments) -> Function.ID {
      let p = program.specialize(z, for: specialization, in: scopeOfUse)
      if let m = program.requirementDeclaring(memberReferredBy: f) {
        return monomorphize(requirement: m.decl, of: m.trait, in: ir, for: p, in: scopeOfUse)
      } else {
        return monomorphize(f, in: ir, for: p, in: scopeOfUse)
      }
    }

    /// Returns a monomorphized copy of `o` for use in `scopeOfUse`.
    func rewritten(_ o: Operand) -> Operand {
      switch o {
      case .constant(let c):
        return .constant(rewritten(c))
      case .parameter(let b, let i):
        return .parameter(rewrittenBlocks[b]!, i)
      case .register(let s):
        return .register(rewrittenInstructions[s]!)
      }
    }
  }

  /// Returns a reference to the monomorphized form of `requirement` for specialization `z` in
  /// `scopeOfUse`, reading definitions from `ir`.
  private mutating func monomorphize(
    requirement: AnyDeclID, of trait: TraitType, in ir: IR.Program,
    for z: GenericArguments, in scopeOfUse: AnyScopeID
  ) -> Function.ID {
    let receiver = ir.base[trait.decl].receiver.id
    let model = z[receiver]!.asType!

    guard let c = ir.base.conformance(of: model, to: trait, exposedTo: scopeOfUse) else {
      fatalError("expected '\(model)' to conform to '\(trait)'")
    }
    let i = c.implementations[requirement]!
    let d = demandDeclaration(lowering: i)

    // Nothing to do if the implementation isn't generic.
    if self[d].genericParameters.isEmpty {
      return d
    }

    // Otherwise, the generic arguments of the implementation are supplied by the conformance,
    // except for the argument to the receiver parameter. This parameter may be associated with
    // a trait other the one declaring the requirement if the implementation is in an extension.
    var monomorphizationArguments = c.arguments
    if case .concrete(let d) = i, let t = ir.base.traitDeclaring(d), t != trait {
      monomorphizationArguments[ir.base[t.decl].receiver] = .type(model)
    } else {
      monomorphizationArguments[receiver] = .type(model)
    }

    return monomorphize(d, in: ir, for: monomorphizationArguments, in: scopeOfUse)
  }

  /// Returns the IR function monomorphizing `f` for `specialization` in `scopeOfUse`.
  private mutating func demandMonomorphizedDeclaration(
    of f: Function.ID, in ir: IR.Program,
    for specialization: GenericArguments, in scopeOfUse: AnyScopeID
  ) -> Function.ID {
    let sourceModule = ir.modules[ir.module(defining: f)]!
    let source = sourceModule[f]

    precondition(!source.genericParameters.isEmpty, "function is not generic")
    precondition(
      source.genericParameters.allSatisfy({ specialization[$0] != nil }),
      "incomplete monomorphization arguments")

    let result = Function.ID(monomorphized: f, for: specialization)
    if functions[result] != nil {
      return result
    }

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

  /// Allocates and initializes storage for each generic value argument in `specialization` in the
  /// entry of `monomorphized`, returning a map from a generic value parameter to its corresponding
  /// allocation that preserves the order of `specialization`.
  private mutating func defineGenericValueArguments(
    _ specialization: GenericArguments,
    in monomorphized: Function.ID
  ) -> OrderedDictionary<GenericParameterDecl.ID, InstructionID> {
    let insertionSite = SourceRange.empty(at: self[monomorphized].site.first())
    let entry = Block.ID(monomorphized, self[monomorphized].entry!)

    var genericValues = OrderedDictionary<GenericParameterDecl.ID, InstructionID>()

    for (k, v) in specialization {
      if v.asType != nil { continue }

      guard let w = v.asCompilerKnown(Int.self) else {
        UNIMPLEMENTED("arbitrary compile-time values")
      }

      let s = append(makeAllocStack(^program.ast.coreType("Int")!, at: insertionSite), to: entry)

      var log = DiagnosticSet()
      Emitter.withInstance(insertingIn: &self, reportingDiagnosticsTo: &log) { (e) in
        e.insertionPoint = .end(of: entry)
        e.emitStore(int: w, to: .register(s), at: insertionSite)
      }
      assert(log.isEmpty)

      genericValues[k] = s
    }

    return genericValues
  }

}

extension TypedProgram {

  /// Returns the generic parameters captured in the scope of `f`.
  fileprivate func liftedGenericParameters(of f: Function.ID) -> [GenericParameterDecl.ID] {
    switch f.value {
    case .lowered(let d):
      return liftedGenericParameters(of: d)

    case .synthesized(let d):
      if let a = BoundGenericType(d.receiver)?.arguments {
        return Array(a.keys)
      } else {
        return []
      }

    case .existentialized, .monomorphized:
      return []
    }
  }

}
