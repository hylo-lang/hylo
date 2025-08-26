import FrontEnd
import Utils

/// A type that transforms the parts of IR instructions.
protocol InstructionTransformer {

  /// Returns a transformed copy of `t` for use in `ir`.
  func transform(_ t: AnyType) -> AnyType

  /// Returns a transformed copy of `o` for use in `ir`.
  func transform(_ o: Operand) -> Operand

  /// Returns a transformed copy of `b` for use in `ir`.
  func transform(_ b: Block.ID) -> Block.ID

}

extension InstructionTransformer {

  /// Returns a transformed copy of `t` for use in `ir`.
  func transform(_ t: IR.`Type`) -> IR.`Type` {
    .init(ast: transform(t.ast), isAddress: t.isAddress)
  }

  /// Returns a transformed copy of the elements in `s` for use in `ir`.
  func transform<S: Sequence<Operand>>(_ s: S) -> [Operand] {
    s.map({ (a) in transform(a) })
  }

}

extension Function {

  /// Inserts a copy of `i`, which is in `f`/`m`, at `p` inside `self`, transforming its parts with
  /// `t` and returning the identifier of the new instruction.
  mutating func rewrite<T: InstructionTransformer>(
    _ i: InstructionID, in f: Function.ID, from m: Module, transformedBy t: inout T,
    at p: InsertionPoint
  ) -> InstructionID {
    switch m[i, in: f] {
    case let s as Access:
      let x0 = t.transform(s.source)
      return makeAccess(s.capabilities, from: x0, at: s.site, insertingAt: p)

    case let s as AddressToPointer:
      let x0 = t.transform(s.source)
      return makeAddressToPointer(x0, at: s.site, insertingAt: p)

    case let s as AdvancedByBytes:
      let x0 = t.transform(s.base)
      let x1 = t.transform(s.byteOffset)
      return makeAdvancedByBytes(source: x0, offset: x1, at: s.site, insertingAt: p)

    case let s as AdvancedByStrides:
      let x0 = t.transform(s.base)
      return makeAdvanced(x0, byStrides: s.offset, at: s.site, insertingAt: p)

    case let s as AllocStack:
      let x0 = t.transform(s.allocatedType)
      return makeAllocStack(x0, at: s.site, insertingAt: p)

    case let s as Branch:
      let x0 = t.transform(s.target)
      return makeBranch(to: x0, at: s.site, insertingAt: p)

    case let s as Call:
      let x0 = t.transform(s.callee)
      let x1 = t.transform(s.arguments)
      let x2 = t.transform(s.output)
      return makeCall(applying: x0, to: x1, writingResultTo: x2, at: s.site, insertingAt: p)

    case let s as CallFFI:
      let x0 = t.transform(s.returnType)
      let x1 = t.transform(s.operands)
      return makeCallFFI(returning: x0, applying: s.callee, to: x1, at: s.site, insertingAt: p)

    case let s as CaptureIn:
      let x0 = t.transform(s.source)
      let x1 = t.transform(s.target)
      return makeCapture(x0, in: x1, at: s.site, insertingAt: p)

    case let s as CloseCapture:
      let x0 = t.transform(s.start)
      return makeCloseCapture(x0, at: s.site, insertingAt: p)

    case let s as CloseUnion:
      let x0 = t.transform(s.start)
      return makeCloseUnion(x0, at: s.site, insertingAt: p)

    case let s as CondBranch:
      let x0 = t.transform(s.condition)
      let x1 = t.transform(s.targetIfTrue)
      let x2 = t.transform(s.targetIfFalse)
      return makeCondBranch(if: x0, then: x1, else: x2, at: s.site, insertingAt: p)

    case let s as ConstantString:
      return insert(s, at: p)

    case let s as DeallocStack:
      let x0 = t.transform(s.location)
      return makeDeallocStack(for: x0, at: s.site, insertingAt: p)

    case let s as EndAccess:
      let x0 = t.transform(s.start)
      return makeEndAccess(x0, at: s.site, insertingAt: p)

    case let s as EndProject:
      let x0 = t.transform(s.start)
      return makeEndProject(x0, at: s.site, insertingAt: p)

    case let s as GenericParameter:
      return insert(s, at: p)

    case let s as GlobalAddr:
      return insert(s, at: p)

    case let s as CallBuiltinFunction:
      let x0 = t.transform(s.operands)
      return makeCallBuiltin(applying: s.callee, to: x0, at: s.site, insertingAt: p)

    case let s as MarkState:
      let x0 = t.transform(s.storage)
      return makeMarkState(x0, initialized: s.initialized, at: s.site, insertingAt: p)

    case let s as MemoryCopy:
      let x0 = t.transform(s.source)
      let x1 = t.transform(s.target)
      return makeMemoryCopy(x0, x1, at: s.site, insertingAt: p)

    case let s as Load:
      let x0 = t.transform(s.source)
      return makeLoad(x0, at: s.site, insertingAt: p)

    case let s as OpenCapture:
      let x0 = t.transform(s.source)
      return makeOpenCapture(x0, at: s.site, insertingAt: p)

    case let s as OpenUnion:
      let x0 = t.transform(s.container)
      let x1 = t.transform(s.payloadType)
      return makeOpenUnion(x0, as: x1, forInitialization: s.isUsedForInitialization, at: s.site, insertingAt: p)

    case let s as PointerToAddress:
      let x0 = t.transform(s.source)
      let x1 = RemoteType(t.transform(^s.target))!
      return makePointerToAddress(x0, to: x1, at: s.site, insertingAt: p)

    case let s as Project:
      let x0 = RemoteType(t.transform(^s.projection))!
      let x1 = t.transform(s.callee)
      let x2 = t.transform(s.arguments)
      return makeProject(
        x0, applying: x1, to: x2,
        at: s.site, insertingAt: p
      )

    case let s as ReleaseCaptures:
      let x0 = t.transform(s.container)
      return makeReleaseCapture(x0, at: s.site, insertingAt: p)

    case let s as Return:
      return insert(s, at: p)

    case let s as Store:
      let x0 = t.transform(s.object)
      let x1 = t.transform(s.target)
      return makeStore(x0, at: x1, at: s.site, insertingAt: p)

    case let s as SubfieldView:
      let x0 = t.transform(s.recordAddress)
      let t = t.transform(s.resultType.ast)
      return makeSubfieldView(
        of: x0, subfield: s.subfield, resultType: t, at: s.site, insertingAt: p
      )

    case let s as Switch:
      let x0 = t.transform(s.index)
      let x1 = s.successors.map({ (b) in t.transform(b) })
      return makeSwitch(on: x0, toOneOf: x1, at: s.site, insertingAt: p)

    case let s as UnionDiscriminator:
      let x0 = t.transform(s.container)
      return makeUnionDiscriminator(x0, at: s.site, insertingAt: p)

    case let s as UnionSwitch:
      let x0 = t.transform(s.discriminator)
      let x1 = UnionType(t.transform(^s.union))!
      let x2 = s.targets.reduce(into: UnionSwitch.Targets()) { (d, kv) in
        _ = d[t.transform(kv.key)].setIfNil(t.transform(kv.value))
      }
      return makeUnionSwitch(over: x0, of: x1, toOneOf: x2, at: s.site, insertingAt: p)

    case let s as Unreachable:
      return insert(s, at: p)

    case let s as Yield:
      let x0 = t.transform(s.projection)
      return makeYield(s.capability, x0, at: s.site, insertingAt: p)

    default:
      unreachable()
    }
  }

}
