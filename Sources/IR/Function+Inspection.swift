import FrontEnd
import Utils

extension Function {

  /// Accesses the instruction denoted by `o` if it is `.register`; returns `nil` otherwise.
  public subscript(o: Operand) -> Instruction? {
    if case .register(let i) = o {
      return self[i]
    } else {
      return nil
    }
  }

  /// The entry of the function.
  public var entry: Block.ID? { blocks.firstAddress.map(Block.ID.init) }

  /// `true` iff the function takes generic parameters.
  public var isGeneric: Bool {
    !genericParameters.isEmpty
  }

  /// Returns the operand representing the return value of `self`.
  public var returnValue: Operand? {
    if !isSubscript, let e = entry {
      return Operand.parameter(e, inputs.count)
    } else {
      return nil
    }
  }

  /// Returns the IDs of the blocks in `self`.
  ///
  /// The first element of the returned collection is the function's entry; other elements are in
  /// no particular order.
  public var blockIDs: LazyMapSequence<Function.Blocks.Indices, Block.ID> {
    blocks.indices.lazy.map({ .init($0.address) })
  }

  /// Returns the IDs of the instructions in `self`, from all the blocks.
  public var instructionIDs:
    LazyMapSequence<
      LazySequence<DoublyLinkedList<any Instruction>.Addresses>.Elements, InstructionID
    >
  {
    instructions.addresses.lazy.map({ InstructionID($0) })
  }

  /// Returns the IDs of the instructions in `b`, in order.
  public func instructions(
    in b: Block.ID
  ) -> some Sequence<InstructionID> {
    var next = blocks[b.address].first
    let last = blocks[b.address].last
    let i = AnyIterator {
      if let n = next {
        next = (n != last) ? InstructionID(instructions.address(after: n.address)!) : nil
        return n
      } else {
        return nil
      }
    }
    return i
  }

  /// Returns the ID of the first instruction in `b`, if any.
  public func firstInstruction(in b: Block.ID) -> InstructionID? {
    self[b].first
  }

  /// Returns the ID the instruction before `i`.
  ///
  /// Note: this may cross block boundaries.
  func instruction(before i: InstructionID) -> InstructionID? {
    instructions.address(before: i.address).map({ InstructionID($0) })
  }

  /// Returns the ID the instruction before `i`.
  func instruction(before i: InstructionID, in b: Block.ID) -> InstructionID? {
    if self[b].first == i { return nil }
    return instructions.address(before: i.address).map({ InstructionID($0) })
  }

  /// Returns the ID the instruction after `i`.
  ///
  /// Note: this may cross block boundaries.
  func instruction(after i: InstructionID) -> InstructionID? {
    instructions.address(after: i.address).map({ InstructionID($0) })
  }

  /// Returns the ID the instruction after `i`, in block `b`.
  func instruction(after i: InstructionID, in b: Block.ID) -> InstructionID? {
    if self[b].last == i { return nil }
    return instructions.address(after: i.address).map({ InstructionID($0) })
  }

  /// Returns the block corresponding to `i`.
  func block(of i: InstructionID) -> Block.ID {
    blockForInstruction[i]!
  }

  /// Returns the block corresponding to `p`.
  internal func block(of p: InsertionPoint) -> Block.ID? {
    switch p {
    case .start(let b):
      return b
    case .end(let b):
      return b
    case .before(let i):
      return blockForInstruction[i]!
    case .after(let i):
      return blockForInstruction[i]!
    }
  }

  /// Returns the scope in which `i` is used.
  public func scope(containing i: InstructionID) -> AnyScopeID {
    self[blockForInstruction[i]!].scope
  }

  /// Returns `true` iff `lhs` is sequenced before `rhs` on all paths leading to `rhs`.
  func dominates(_ lhs: InstructionID, _ rhs: InstructionID) -> Bool {
    // Fast path: both instructions are in the same block.
    if blockForInstruction[lhs]! == blockForInstruction[rhs]! {
      return lhs.address.precedes(rhs.address, in: instructions)
    }

    // Slow path: use the dominator tree.
    let d = DominatorTree(function: self, cfg: cfg())
    return d.dominates(blockForInstruction[lhs]!.address, blockForInstruction[rhs]!.address)
  }

  /// Returns `true` iff `lhs` is sequenced before `rhs` in the block of `lhs`.
  /// If `lhs` and `rhs` are in different blocks, this function will return `false`.
  public func precedes(_ lhs: InstructionID, _ rhs: InstructionID) -> Bool {
    if blockForInstruction[lhs]! != blockForInstruction[rhs]! {
      return false
    }
    return lhs.address.precedes(rhs.address, in: instructions)
  }

  /// Returns the global identity of `block`'s terminator, if it exists.
  func terminator(of block: Block.ID) -> InstructionID? {
    self[block].last
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
      return self[i].result!
    case .parameter(let b, let n):
      return blocks[b.address].inputs[n]
    case .constant(let c):
      return c.type
    }
  }

  /// Returns `true` iff `p` cannot be used to modify or update a value.
  public func isBoundImmutably(_ p: Operand) -> Bool {
    switch p {
    case .parameter(let e, let i):
      return (entry == e) && (passingConvention(parameter: i) == .let)
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
    if case .parameter(let e, let i) = p, entry == e {
      return passingConvention(parameter: i)
    } else {
      return nil
    }
  }

  /// Returns the passing convention of the `i`-th parameter of `self`.
  public func passingConvention(parameter i: Int) -> AccessEffect {
    // The last parameter of a function denotes its return value.
    return (i == inputs.count) ? .set : inputs[i].type.access
  }

  /// Returns the uses of all the registers assigned by `i`.
  func allUses(of i: InstructionID) -> [Use] {
    result(of: i).map(default: [], { uses[$0, default: []] })
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
      return s.arguments.reduce(into: []) { (p, o) in
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
        if entry == e {
          return inputs[i].type.access == .sink
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
