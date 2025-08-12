import FrontEnd
import Utils

/// A type that can inspect the IR of a function, based on given IDs.
public protocol IRInspector {

  /// Returns the IR of the function denoted by `f`.
  func ir(for f: Function.ID) -> Function

}

extension IRInspector {

  /// Accesses the given function.
  public subscript(f: Function.ID) -> Function {
    ir(for: f)
  }

  /// Accesses the given block.
  public subscript(b: Block.ID) -> Block {
    ir(for: b.function).blocks[b.address]
  }

  /// Accesses the given instruction.
  public subscript(i: InstructionID) -> Instruction {
    ir(for: i.function).blocks[i.block].instructions[i.address]
  }

  /// Accesses the instruction denoted by `o` if it is `.register`; returns `nil` otherwise.
  public subscript(o: Operand) -> Instruction? {
    if case .register(let i) = o {
      return self[i]
    } else {
      return nil
    }
  }

  /// Returns the IDs of the blocks in `f`.
  ///
  /// The first element of the returned collection is the function's entry; other elements are in
  /// no particular order.
  public func blocks(
    in f: Function.ID
  ) -> LazyMapSequence<Function.Blocks.Indices, Block.ID> {
    ir(for: f).blocks.indices.lazy.map({ .init(f, $0.address) })
  }

  /// Returns the entry of `f`.
  ///
  /// - Requires: `f` is declared in `self`.
  public func entry(of f: Function.ID) -> Block.ID? {
    ir(for: f).entry.map { Block.ID(f, $0) }
  }

  /// Returns the IDs of the instructions in `b`, in order.
  public func instructions(
    in b: Block.ID
  ) -> LazyMapSequence<Block.Instructions.Indices, InstructionID> {
    self[b].instructions.indices.lazy.map({ .init(b.function, b.address, $0.address) })
  }

  /// Returns the ID the instruction before `i`.
  func instruction(before i: InstructionID) -> InstructionID? {
    ir(for: i.function)[i.block].instructions.address(before: i.address)
      .map({ InstructionID(i.function, i.block, $0) })
  }

  /// Returns the ID the instruction after `i`.
  func instruction(after i: InstructionID) -> InstructionID? {
    ir(for: i.function)[i.block].instructions.address(after: i.address)
      .map({ InstructionID(i.function, i.block, $0) })
  }

  /// Returns the scope in which `i` is used.
  public func scope(containing i: InstructionID) -> AnyScopeID {
    ir(for: i.function)[i.block].scope
  }

  /// Returns `true` iff `lhs` is sequenced before `rhs`.
  func dominates(_ lhs: InstructionID, _ rhs: InstructionID) -> Bool {
    if lhs.function != rhs.function { return false }

    // Fast path: both instructions are in the same block.
    if lhs.block == rhs.block {
      let sequence = ir(for: lhs.function)[lhs.block].instructions
      return lhs.address.precedes(rhs.address, in: sequence)
    }

    // Slow path: use the dominator tree.
    let d = DominatorTree(function: ir(for: lhs.function))
    return d.dominates(lhs.block, rhs.block)
  }

  /// Returns the global identity of `block`'s terminator, if it exists.
  func terminator(of block: Block.ID) -> InstructionID? {
    if let a = self[block].instructions.lastAddress {
      return InstructionID(block, a)
    } else {
      return nil
    }
  }

  /// Returns the operand representing the return value of `f`.
  ///
  /// - Requires: `f` is declared in `self`.
  public func returnValue(of f: Function.ID) -> Operand? {
    let i = ir(for: f)
    if !i.isSubscript, let e = entry(of: f) {
      return Operand.parameter(e, i.inputs.count)
    } else {
      return nil
    }
  }

  /// Returns the register assigned by `i`, if any.
  func result(of i: InstructionID) -> Operand? {
    if self[i].result != nil {
      return .register(i)
    } else {
      return nil
    }
  }

  /// Returns the type of `operand`.
  public func type(of operand: Operand) -> IR.`Type` {
    switch operand {
    case .register(let i):
      return ir(for: i.function)[i.block][i.address].result!
    case .parameter(let b, let n):
      return ir(for: b.function)[b.address].inputs[n]
    case .constant(let c):
      return c.type
    }
  }

  /// Returns `true` iff cannot be used to modify or update a value.
  public func isBoundImmutably(_ p: Operand) -> Bool {
    switch p {
    case .parameter(let e, let i):
      let f = e.function
      return (entry(of: f) == e) && (passingConvention(parameter: i, of: f) == .let)
    case .constant:
      return false
    case .register(let i):
      return isBoundImmutably(register: i)
    }
  }

  /// Returns `true` iff the result of `i` cannot be used to modify or update a value.
  public func isBoundImmutably(register i: InstructionID) -> Bool {
    switch self[i] {
    case is AllocStack:
      return false
    case let s as AdvancedByBytes:
      return isBoundImmutably(s.base)
    case let s as AdvancedByStrides:
      return isBoundImmutably(s.base)
    case let s as Access:
      return isBoundImmutably(s.source)
    case let s as OpenCapture:
      return s.isAccess(.let)
    case is OpenUnion:
      return false
    case let s as PointerToAddress:
      return s.isAccess(.let)
    case let s as Project:
      return s.projection.access == .let
    case let s as SubfieldView:
      return isBoundImmutably(s.recordAddress)
    case let s as WrapExistentialAddr:
      return isBoundImmutably(s.witness)
    default:
      return true
    }
  }

  /// If `p` is a function parameter, returns its passing convention. Otherwise, returns `nil`.
  public func passingConvention(of p: Operand) -> AccessEffect? {
    if case .parameter(let e, let i) = p, entry(of: e.function) == e {
      return passingConvention(parameter: i, of: e.function)
    } else {
      return nil
    }
  }

  /// Returns the passing convention of the `i`-th parameter of `f`.
  public func passingConvention(parameter i: Int, of f: Function.ID) -> AccessEffect {
    // The last parameter of a function denotes its return value.
    let ps = self[f].inputs
    return (i == ps.count) ? .set : ps[i].type.access
  }

  /// Returns the uses of all the registers assigned by `i`.
  func allUses(of i: InstructionID) -> [Use] {
    result(of: i).map(default: [], { ir(for: i.function).uses[$0, default: []] })
  }

  /// Returns the operands from which the address denoted by `a` derives.
  ///
  /// The (static) provenances of an address denote the original operands from which it derives.
  /// They form a set because an address computed by a projection depends on that projection's
  /// arguments and because an address defined as a parameter of a basic block with multiple
  /// predecessors depends on that bock's arguments.
  func provenances(_ a: Operand) -> Set<Operand> {
    // TODO: Block arguments
    guard let i = a.instruction else { return [a] }

    switch self[i] {
    case let s as AdvancedByBytes:
      return provenances(s.base)
    case let s as Access:
      return provenances(s.source)
    case let s as Project:
      return s.operands.reduce(into: []) { (p, o) in
        if type(of: o).isAddress { p.formUnion(provenances(o)) }
      }
    case let s as SubfieldView:
      return provenances(s.recordAddress)
    case let s as WrapExistentialAddr:
      return provenances(s.witness)
    default:
      return [a]
    }
  }

  /// Returns `true` if `o` can be sunken.
  func isSink(_ o: Operand) -> Bool {
    provenances(o).allSatisfy { (p) -> Bool in
      switch p {
      case .parameter(let e, let i):
        if entry(of: e.function) == e {
          return self[e.function].inputs[i].type.access == .sink
        } else {
          return false
        }

      case .register(let i):
        switch self[i] {
        case let s as ProjectBundle:
          return s.capabilities.contains(.sink)
        case let s as Project:
          return s.projection.access == .sink
        default:
          return true
        }

      default:
        return true
      }
    }
  }

  /// Returns `true` iff `o` is an `access [set]` instruction.
  func isBorrowSet(_ o: Operand) -> Bool {
    guard
      let i = o.instruction,
      let s = self[i] as? Access
    else { return false }
    return s.capabilities == .set
  }

}
