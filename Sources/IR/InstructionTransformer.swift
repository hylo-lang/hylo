import FrontEnd
import Utils

/// A type that transforms the parts of IR instructions.
protocol InstructionTransformer {

  /// Returns a transformed copy of `t` for use in `ir`.
  func transform(_ t: AnyType, in ir: inout IR.Program) -> AnyType

  /// Returns a transformed copy of `o` for use in `ir`.
  func transform(_ o: Operand, in ir: inout IR.Program) -> Operand

  /// Returns a transformed copy of `b` for use in `ir`.
  func transform(_ b: Block.ID, in ir: inout IR.Program) -> Block.ID

}

extension InstructionTransformer {

  /// Returns a transformed copy of `t` for use in `ir`.
  func transform(_ t: IR.`Type`, in ir: inout IR.Program) -> IR.`Type` {
    .init(ast: transform(t.ast, in: &ir), isAddress: t.isAddress)
  }

  /// Returns a transformed copy of the elements in `s` for use in `ir`.
  func transform<S: Sequence<Operand>>(_ s: S, in ir: inout IR.Program) -> [Operand] {
    s.map({ (a) in transform(a, in: &ir) })
  }

}

extension IR.Program {

  /// Inserts a copy of `i`, which is in `m`, at `p`, which is in `n`, transforming its parts with
  /// `t` and returning its identifier.
  mutating func rewrite<T: InstructionTransformer>(
    _ i: InstructionID, in f: Function.ID, from m: Module.ID, transformedBy t: inout T,
    at p: InsertionPoint, targeting g: Function.ID, in n: Module.ID
  ) -> InstructionID {
    switch modules[m]![i, in: f] {
    case let s as Access:
      let x0 = t.transform(s.source, in: &self)
      return insert(at: p, in: g, in: n) { (target) in
        Access(
          capabilities: s.capabilities,
          accessedType: target[g].type(of: x0),
          source: x0,
          site: s.site)
      }

    case let s as AddressToPointer:
      let x0 = t.transform(s.source, in: &self)
      return insert(at: p, in: g, in: n) { (target) in
        AddressToPointer(source: x0, site: s.site)
      }

    case let s as AdvancedByBytes:
      let x0 = t.transform(s.base, in: &self)
      let x1 = t.transform(s.byteOffset, in: &self)
      return insert(at: p, in: g, in: n) { (target) in
        AdvancedByBytes(source: x0, offset: x1, site: s.site)
      }

    case let s as AdvancedByStrides:
      let x0 = t.transform(s.base, in: &self)
      return insert(at: p, in: g, in: n) { (target) in
        AdvancedByStrides(source: x0, offset: s.offset, result: target[g].type(of: x0), site: s.site)
      }

    case let s as AllocStack:
      let x0 = t.transform(s.allocatedType, in: &self)
      return insert(at: p, in: g, in: n) { (target) in
        AllocStack(allocatedType: x0, site: s.site)
      }

    case let s as Branch:
      let x0 = t.transform(s.target, in: &self)
      return insert(at: p, in: g, in: n) { (target) in
        Branch(target: x0, site: s.site)
      }

    case let s as Call:
      let x0 = t.transform(s.callee, in: &self)
      let x1 = t.transform(s.arguments, in: &self)
      let x2 = t.transform(s.output, in: &self)
      return insert(at: p, in: g, in: n) { (target) in
        Call(callee: x0, output: x2, arguments: x1, site: s.site)
      }

    case let s as CallFFI:
      let x0 = t.transform(s.returnType, in: &self)
      let x1 = t.transform(s.operands, in: &self)
      return insert(at: p, in: g, in: n) { (target) in
        CallFFI(returnType: x0, callee: s.callee, arguments: x1, site: s.site)
      }

    case let s as CaptureIn:
      let x0 = t.transform(s.source, in: &self)
      let x1 = t.transform(s.target, in: &self)
      return insert(at: p, in: g, in: n) { (target) in
        CaptureIn(source: x0, target: x1, site: s.site)
      }

    case let s as CloseCapture:
      let x0 = t.transform(s.start, in: &self)
      return insert(at: p, in: g, in: n) { (target) in
        let ir = target[g]
        precondition(x0.instruction.map({ ir[$0] is OpenCapture }) ?? false)
        return CloseCapture(start: x0, site: s.site)
      }

    case let s as CloseUnion:
      let x0 = t.transform(s.start, in: &self)
      return insert(at: p, in: g, in: n) { (target) in
        CloseUnion(start: x0, site: s.site)
      }

    case let s as CondBranch:
      let x0 = t.transform(s.condition, in: &self)
      let x1 = t.transform(s.targetIfTrue, in: &self)
      let x2 = t.transform(s.targetIfFalse, in: &self)
      return insert(at: p, in: g, in: n) { (target) in
        CondBranch(condition: x0, targetIfTrue: x1, targetIfFalse: x2, site: s.site)
      }

    case let s as ConstantString:
      return modules[n]![g].insert(s, at: p)

    case let s as DeallocStack:
      let x0 = t.transform(s.location, in: &self)
      return insert(at: p, in: g, in: n) { (target) in
        DeallocStack(location: x0, site: s.site)
      }

    case let s as EndAccess:
      let x0 = t.transform(s.start, in: &self)
      return insert(at: p, in: g, in: n) { (target) in
        EndAccess(start: x0, site: s.site)
      }

    case let s as EndProject:
      let x0 = t.transform(s.start, in: &self)
      return insert(at: p, in: g, in: n) { (target) in
        EndProject(start: x0, site: s.site)
      }

    case let s as GenericParameter:
      return modules[n]![g].insert(s, at: p)

    case let s as GlobalAddr:
      return modules[n]![g].insert(s, at: p)

    case let s as CallBuiltinFunction:
      let x0 = t.transform(s.operands, in: &self)
      return insert(at: p, in: g, in: n) { (target) in
        CallBuiltinFunction(applying: s.callee, to: x0, site: s.site)
      }

    case let s as MarkState:
      let x0 = t.transform(s.storage, in: &self)
      return insert(at: p, in: g, in: n) { (target) in
        MarkState(storage: x0, initialized: s.initialized, site: s.site)
      }

    case let s as MemoryCopy:
      let x0 = t.transform(s.source, in: &self)
      let x1 = t.transform(s.target, in: &self)
      return insert(at: p, in: g, in: n) { (target) in
        MemoryCopy(source: x0, target: x1, site: s.site)
      }

    case let s as Load:
      let x0 = t.transform(s.source, in: &self)
      return insert(at: p, in: g, in: n) { (target) in
        Load(objectType: .object(target[g].type(of: x0).ast), from: x0, site: s.site)
      }

    case let s as OpenCapture:
      let x0 = t.transform(s.source, in: &self)
      return insert(at: p, in: g, in: n) { (target) in
        let t = RemoteType(target[g].type(of: x0).ast) ?? preconditionFailure()
        return OpenCapture(result: .address(t.bareType), source: x0, site: s.site)
      }

    case let s as OpenUnion:
      let x0 = t.transform(s.container, in: &self)
      let x1 = t.transform(s.payloadType, in: &self)
      return insert(at: p, in: g, in: n) { (target) in
        OpenUnion(container: x0, payloadType: x1, isUsedForInitialization: s.isUsedForInitialization, site: s.site)
      }

    case let s as PointerToAddress:
      let x0 = t.transform(s.source, in: &self)
      let x1 = RemoteType(t.transform(^s.target, in: &self))!
      return insert(at: p, in: g, in: n) { (target) in
        PointerToAddress(source: x0, target: x1, site: s.site)
      }

    case let s as Project:
      let r = FunctionReference(
        to: s.callee, in: modules[m]!,
        specializedBy: s.specialization, in: modules[m]![f].scope(containing: i))
      let oldCallee = Operand.constant(r)
      let newCallee = t.transform(oldCallee, in: &self).constant as! FunctionReference

      let x0 = RemoteType(t.transform(^s.projection, in: &self))!
      let x1 = t.transform(s.operands, in: &self)
      return insert(at: p, in: g, in: n) { (target) in
        Project(
          projection: x0, callee: newCallee.function, specialization: newCallee.specialization, operands: x1,
          site: s.site)
      }

    case let s as ReleaseCaptures:
      let x0 = t.transform(s.container, in: &self)
      return insert(at: p, in: g, in: n) { (target) in
        ReleaseCaptures(container: x0, site: s.site)
      }

    case let s as Return:
      return modules[n]![g].insert(s, at: p)

    case let s as Store:
      let x0 = t.transform(s.object, in: &self)
      let x1 = t.transform(s.target, in: &self)
      return insert(at: p, in: g, in: n) { (target) in
        Store(object: x0, at: x1, site: s.site)
      }

    case let s as SubfieldView:
      let x0 = t.transform(s.recordAddress, in: &self)
      return insert(at: p, in: g, in: n) { (target) in
        let ir = target[g]
        precondition(ir.type(of: x0).isAddress)
        let l = AbstractTypeLayout(of: ir.type(of: x0).ast, definedIn: target.program)
        let t = l[s.subfield].type
        return SubfieldView(base: x0, subfield: s.subfield, subfieldType: .address(t), site: s.site)
      }

    case let s as Switch:
      let x0 = t.transform(s.index, in: &self)
      let x1 = s.successors.map({ (b) in t.transform(b, in: &self) })
      return insert(at: p, in: g, in: n) { (target) in
        let t = target[g].type(of: x0)
        precondition(t.isObject && t.ast.isBuiltinInteger)
        precondition(!x1.isEmpty)

        return Switch(index: x0, successors: x1, site: s.site)
      }

    case let s as UnionDiscriminator:
      let x0 = t.transform(s.container, in: &self)
      return insert(at: p, in: g, in: n) { (target) in
        UnionDiscriminator(container: x0, site: s.site)
      }

    case let s as UnionSwitch:
      let x0 = t.transform(s.discriminator, in: &self)
      let x1 = UnionType(t.transform(^s.union, in: &self))!
      let x2 = s.targets.reduce(into: UnionSwitch.Targets()) { (d, kv) in
        _ = d[t.transform(kv.key, in: &self)].setIfNil(t.transform(kv.value, in: &self))
      }
      return insert(at: p, in: g, in: n) { (target) in
        UnionSwitch(discriminator: x0, union: x1, targets: x2, site: s.site)
      }

    case let s as Unreachable:
      return modules[n]![g].insert(s, at: p)

    case let s as Yield:
      let x0 = t.transform(s.projection, in: &self)
      return insert(at: p, in: g, in: n) { (target) in
        Yield(capability: s.capability, projection: x0, site: s.site)
      }

    default:
      unreachable()
    }
  }

  /// Inserts the result of `makeInstruction` at `p`, which is in `m`.
  private mutating func insert<T: Instruction>(
    at p: InsertionPoint, in f: Function.ID, in m: Module.ID, _ makeInstruction: (inout Module) -> T
  ) -> InstructionID {
    modify(&modules[m]!) { (x) in
      let s = makeInstruction(&x)
      return x[f].insert(s, at: p)
    }
  }

}
