import FrontEnd
import Utils

/// A type that observes the parts of IR instructions.
protocol InstructionObserver {

  /// Observes `t`.
  mutating func observe(_ t: AnyType)

  /// Observes `o`.
  mutating func observe(_ o: Operand)

  /// Observes `b`.
  mutating func observe(_ b: Block.ID)

}

extension InstructionObserver {

  /// Observes `t`.
  mutating func observe(_ t: IR.`Type`) {
    observe(t.ast)
  }

  /// Observes the elements in `s`.
  mutating func observe<S: Sequence<Operand>>(_ s: S) {
    for a in s {
      observe(a)
    }
  }

}

extension Function {

  /// Observes the parts of `i` with `o`.
  mutating func observe<T: InstructionObserver>(_ i: InstructionID, with o: inout T) {
    switch self[i] {
    case let s as Access:
      o.observe(s.source)
      break

    case let s as AddressToPointer:
      o.observe(s.source)
      break

    case let s as AdvancedByBytes:
      o.observe(s.base)
      o.observe(s.byteOffset)
      break

    case let s as AdvancedByStrides:
      o.observe(s.base)
      break

    case let s as AllocStack:
      o.observe(s.allocatedType)
      break

    case let s as Branch:
      o.observe(s.target)
      break

    case let s as Call:
      o.observe(s.callee)
      o.observe(s.arguments)
      o.observe(s.output)
      break

    case let s as CallFFI:
      o.observe(s.returnType)
      o.observe(s.operands)
      break

    case let s as CaptureIn:
      o.observe(s.source)
      o.observe(s.target)
      break

    case let s as CloseCapture:
      o.observe(s.start)
      break

    case let s as CloseUnion:
      o.observe(s.start)
      break

    case let s as CondBranch:
      o.observe(s.condition)
      o.observe(s.targetIfTrue)
      o.observe(s.targetIfFalse)
      break

    case _ as ConstantString:
      break

    case let s as DeallocStack:
      o.observe(s.location)
      break

    case let s as EndAccess:
      o.observe(s.start)
      break

    case let s as EndProject:
      o.observe(s.start)
      break

    case _ as GenericParameter:
      break

    case _ as GlobalAddr:
      break

    case let s as CallBuiltinFunction:
      o.observe(s.operands)
      break

    case let s as MarkState:
      o.observe(s.storage)
      break

    case let s as MemoryCopy:
      o.observe(s.source)
      o.observe(s.target)
      break

    case let s as Load:
      o.observe(s.source)
      break

    case let s as OpenCapture:
      o.observe(s.source)
      break

    case let s as OpenUnion:
      o.observe(s.container)
      o.observe(s.payloadType)
      break

    case let s as PointerToAddress:
      o.observe(s.source)
      o.observe(^s.target)
      break

    case let s as Project:
      o.observe(^s.projection)
      o.observe(s.operands)
      break

    case let s as ReleaseCaptures:
      o.observe(s.container)
      break

    case _ as Return:
      break

    case let s as Store:
      o.observe(s.object)
      o.observe(s.target)
      break

    case let s as SubfieldView:
      o.observe(s.recordAddress)
      o.observe(s.resultType)
      break

    case let s as Switch:
      o.observe(s.index)
      for b in s.successors {
        o.observe(b)
      }
      break

    case let s as UnionDiscriminator:
      o.observe(s.container)
      break

    case let s as UnionSwitch:
      o.observe(s.discriminator)
      o.observe(^s.union)
      for (key, value) in s.targets {
        o.observe(key)
        o.observe(value)
      }
      break

    case _ as Unreachable:
      break

    case let s as Yield:
      o.observe(s.projection)
      break

    default:
      unreachable()
    }
  }

  /// Observes the parts of instructions in `s` with `o`.
  mutating func observe<T: InstructionObserver, S: Sequence<InstructionID>>(_ s: S, with o: inout T)
  {
    for i in s {
      observe(i, with: &o)
    }
  }

}
